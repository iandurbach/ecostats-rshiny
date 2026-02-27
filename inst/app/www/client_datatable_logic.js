const dateLimits = {"from" : null, "to" : null};
var checkboxState = {};
var spectroImages = {};
var filteredRows = {};

function hasTableLoaded() {
  return ($.fn.DataTable) && $.fn.DataTable.isDataTable("#match_calls_1-unmatched_calls #DataTables_Table_0");
}
function getTable() {
  return $("#match_calls_1-unmatched_calls #DataTables_Table_0").DataTable();
}


$( document ).ready(function() {

  Shiny.addCustomMessageHandler('showDateTimeSlider', function(e) {
    //Find the datetime slider element and display it.
    $("div#match_calls_1-datetime_slider_container")[0].style.visibility = "visible";
  });


  Shiny.addCustomMessageHandler('hideTableRows', function(r) {
    const rows = (typeof r == "number") ? [r] : r;
    console.log(`rows to hide: ${rows}`);
    rows.forEach(row => filteredRows[row] = true);

    if (hasTableLoaded()) {
        const table = getTable();
      // Uncheck all checkboxes
      table.rows().every(function () {
          var rowId = this.index();
          var checkbox = $(this.node()).find('input[type="checkbox"]').prop('checked', false);
      });
      //Empty the checkboxState
      checkboxState = {};
      // Notify the server that there are no checked rows
       Shiny.setInputValue("match_calls_1-checked_rows", []);
       // Re-draw the table to apply the filter.
      table.draw(false);
    }
  });

  Shiny.addCustomMessageHandler('showTableRows', function(r) {
    const rows = (typeof r == "number") ? [r] : r;
    rows.forEach(row => delete filteredRows[row]);
    if (hasTableLoaded()) {
      getTable().draw(false);
    }
  });



  Shiny.addCustomMessageHandler('filterTableByDate', function(d) {
    // Update the date limit values.
    dateLimits.from = d.from;
    dateLimits.to = d.to;
    if (hasTableLoaded()) {
      //Redraw the table;
      getTable().draw(false);
    }
  });

  Shiny.addCustomMessageHandler('updateTableSpectrogramImages', function(arr) {
    for (const imageObj of arr) {
      //Update the state of spectrogram images
      spectroImages[imageObj.rowId] = imageObj.src;
    }
    // Update the images if the table has loaded
    if (hasTableLoaded()) {
      refreshTableImages(getTable());
    }
  });
});

function refreshTableImages(table) {
  console.log("REFRESHING IMAGES");
  console.log(Object.keys(spectroImages))
  let invalidated = false;
  table.column("spectrogram:name").nodes().each(function(cell, i) {
    if (typeof spectroImages[i] !== "undefined") {
      table.cell(cell).data(`<img src=${spectroImages[i]} width="50px">`);
      table.cell(cell).invalidate();
      invalidated = true;
    }
  });
  if (invalidated) {
    table.draw(false);
  }
}

function registerCheckboxListeners(table) {
  //table.column("tick:name").nodes().to$().find("input").on("click", function() {console.log("clicked: "+ $(this).attr("id"));}); // Individual listeners for every checkboxes

  //Delegated click event handler on the datatable parent
  table.on('click', 'input[type="checkbox"]', function() {
    console.log("clicked: " + $(this).attr("id"));
    const checkId = $(this).attr("id");
    const isChecked = $(this).prop("checked");
    let rowId = checkId.split("_")[1]
    rowId = parseInt(rowId)-1;
    if (isChecked) {
      //Store state
      checkboxState[rowId] = true;
    } else {
      // Only keep checked rows in object.
      delete checkboxState[rowId];
    }
    console.log(checkboxState);

    //Notify the server which rows have been selected
    // +1 since R uses 1-based indexing
    const checkedRows = Object.keys(checkboxState).map(row => parseInt(row)+1);
    Shiny.setInputValue("match_calls_1-checked_rows", checkedRows);
  });

}

function onTableLoadFinish(table) {
  // Copy images to DOM from internal state
  refreshTableImages(table);

  //Register event listers for all checkboxes to keep track of checked state
   registerCheckboxListeners(table);


  // Prevent the first column from being selectable
  table.on('user-select', function(e, dt, type, cell, originalEvent) {
    console.log("==USER-SELECT EVENT==")
   if (cell.index().column === 2) {
    e.preventDefault();
   }
 });
  table.on('select', function(e, dt, type, indexes) {
      if (type === 'row') {

          // Backup all checkboxes
          //storeCheckboxState(table);

          const selectedData = table.rows(indexes).data()[0];
          const selectedToa = selectedData[table.column("toa:name").index()];

          // Iterate through all rows to calculate the time difference
          table.rows().every(function (rowIdx, tableLoop, rowLoop) {
              const rowData = this.data();
              const rowToa = rowData[table.column("toa:name").index()];

              // Calculate the time difference between the selected row and the current row
              const timeDifference = calculateDateTimeDifference(selectedToa, rowToa);

              // Update the timediff column with the calculated time difference
              rowData[table.column("timediff:name").index()] = timeDifference;
              this.data(rowData).invalidate();
          });


          // Redraw the table to reflect the changes
          table.draw(false);

          // Restore all checked boxes
          restoreCheckboxState(table);
      }
  });

  table.on('deselect',  function(e, dt, type, indexes) {

    // Backup all checkboxes
    //storeCheckboxState(table);

    // Loop through all rows
    table.rows().every(function (rowIdx, tableLoop, rowLoop) {
       const rowData = this.data();

      // Set the timediff column equal to "-"
      rowData[table.column("timediff:name").index()] = "-";

      this.data(rowData).invalidate();
    });


    // Redraw the table to reflect the changes
    table.draw(false);

    // Restore all checked boxes
    restoreCheckboxState(table);
  })


  //Custom filtering function that checks whether min <= toa <= max
  // This took AGES to get right!
  // I was sending non ISO-8601 compliant date strings from the server
  // and no error was thrown on the client!
  // Also, this function is a relic buried deep inside old datatable forums
  // Source: https://datatables.net/forums/discussion/7207/fn-datatableext-afnfiltering-push-what-is-this
  // Source2: https://github.com/DataTables/Plugins/blob/master/filtering/row-based/range_dates.js
  // DO NOT TOUCH!
   $.fn.dataTableExt.afnFiltering.push(function(settings, data, dataIndex) {
   if (dateLimits === undefined || filteredRows === undefined || dateLimits.from === null || dateLimits.to === null) {
      return true;
    }
    if (dataIndex in filteredRows) {
      return false;
    }
    const tableApi = new $.fn.dataTable.Api(settings);
    const toaIndex = tableApi.column("toa:name").index();
    if (toaIndex === undefined || toaIndex === null || toaIndex < 0) {
      return true;
    }
    var from = new Date(dateLimits.from);
    var to = new Date(dateLimits.to);
    var iterDate = new Date(data[toaIndex]); // use data from the toa column
    return ((iterDate >= from) && (iterDate <= to)); //keep row only if it is in range.
  })

}

  function calculateDateTimeDifference(dateTime1, dateTime2) {
      // Parse the input date-time strings into Date objects
      const date1 = new Date(dateTime1);
      const date2 = new Date(dateTime2);

      // Check if the dates are valid
      if (isNaN(date1.getTime()) || isNaN(date2.getTime())) {
          throw new Error("Invalid date format");
      }

      const sign = ((date2 - date1) >= 0) ? "+" : "-";
      // Calculate the difference in milliseconds
      const differenceInMillis = Math.abs(date2 - date1);

      // Calculate the difference in seconds
      const differenceInSeconds = Math.floor(differenceInMillis / 1000);

      // Calculate hours, minutes, and seconds
      const hours = Math.floor(differenceInSeconds / 3600);
      const minutes = Math.floor((differenceInSeconds % 3600) / 60);
      const seconds = differenceInSeconds % 60;

      // Format the output based on the values
      if (hours > 0) {
          return `${sign}${hours}h ${minutes}m ${seconds}s`;
      } else if (minutes > 0) {
          return `${sign}${minutes}m ${seconds}s`;
      } else {
          return `${sign}${seconds}s`;
      }
  }

function storeCheckboxState(table) {
  // Store the state of current checkboxes
  table.rows().every(function () {
      var rowId = this.index();
      var isChecked = $(this.node()).find('input[type="checkbox"]').prop('checked');
      checkboxState[rowId] = isChecked;
  });
}

function restoreCheckboxState(table) {
    // Restore the state of checkboxes
    table.rows().every(function () {
        var rowId = this.index();
        var isChecked = checkboxState[rowId];
        if (isChecked !== undefined) {
            $(this.node()).find('input[type="checkbox"]').prop('checked', isChecked);
        }
    });
}

/*
* This function is called whenever the X button is pressed, to remove a call from a group.
* We extract row and group ids from the button id and
* notify the shiny server which calls to remove.
*/
function onRemoveCallFromGroupBtnClick(btn) {
  let ids = btn.id.split("_");
  //ids = [<row_id>, <group_id>]
  ids = [ids[2], ids[4]].map(id => parseInt(id));
  console.log(`Removing row ${ids[0]} from group ${ids[1]}`);
  // Send time to invalidate the input every time; even if the input is the same as previously (disable caching).
  Shiny.setInputValue("match_calls_1-remove_call_from_group", {"rows": ids, "time": (new Date()).getTime()});
}
