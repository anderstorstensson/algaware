// Gallery interaction JavaScript for algaware
// Adapted from ClassiPyR's gallery.js
//
// This file provides the client-side behaviour for the image gallery: image
// selection (click, drag-select, page-sync) and the on-screen measurement
// tool. It communicates with the R server (R/mod_gallery.R) over two channels.
//
// R -> JS  (R calls session$sendCustomMessage(<name>, ...), handled below):
//   "toggle-toolbar-height"  add/remove the .has-data class on the toolbar
//   "measureMode"            enable/disable the measurement tool
//   "updatePixelsPerMicron"  push the camera calibration factor for measuring
//   "toggleMeasureBtn"       style the Measure button as active/inactive
//   "syncSelection"          replace the selected set (select-page / clear-all)
//
// JS -> R  (Shiny.setInputValue(<name>, ...), read as input$<...> in R):
//   "gallery-toggle_image"   {img}     a single image was clicked
//   "gallery-drag_select"    {images}  a rubber-band drag selected several
//   ("gallery-" is the module namespace; in mod_gallery.R these arrive as
//    input$toggle_image and input$drag_select.)

$(document).ready(function() {
  // Toggle toolbar min-height when data loads/unloads
  Shiny.addCustomMessageHandler('toggle-toolbar-height', function(action) {
    var el = document.getElementById('toolbar-class-container');
    if (el) {
      if (action === 'add') {
        el.classList.add('has-data');
      } else {
        el.classList.remove('has-data');
      }
    }
  });

  var wasDragging = false;
  var startX, startY;
  var selectionBox = null;

  // ============================================================================
  // Image error handling (delegated -- no inline onerror needed)
  // ============================================================================
  $(document).on('error', '.image-card img', function() {
    $(this).hide().next('.image-placeholder').show();
  });

  // ============================================================================
  // Measure Tool
  // ============================================================================
  var measureMode = false;
  var measureStart = null;
  var measureLine = null;
  var measureLabel = null;
  var pixelsPerMicron = 3.4;

  Shiny.addCustomMessageHandler('measureMode', function(enabled) {
    measureMode = enabled;
    if (!measureMode) {
      removeMeasureLine();
    }
    if (measureMode) {
      $('.gallery-drag-area').addClass('measure-mode');
    } else {
      $('.gallery-drag-area').removeClass('measure-mode');
    }
  });

  Shiny.addCustomMessageHandler('updatePixelsPerMicron', function(value) {
    pixelsPerMicron = value;
  });

  Shiny.addCustomMessageHandler('toggleMeasureBtn', function(msg) {
    var btn = $('#' + msg.id);
    if (msg.active) {
      btn.removeClass('btn-outline-secondary').addClass('btn-primary');
    } else {
      btn.removeClass('btn-primary').addClass('btn-outline-secondary');
    }
  });

  function removeMeasureLine() {
    if (measureLine) {
      measureLine.remove();
      measureLine = null;
    }
    if (measureLabel) {
      measureLabel.remove();
      measureLabel = null;
    }
    measureStart = null;
  }

  // Measure - mousedown on image
  $(document).on('mousedown', '.image-card img', function(e) {
    if (!measureMode) return;
    e.preventDefault();
    e.stopPropagation();

    removeMeasureLine();

    var container = $('.gallery-drag-area');
    if (container.css('position') === 'static') {
      container.css('position', 'relative');
    }
    var containerOffset = container.offset();

    var relX = e.pageX - containerOffset.left;
    var relY = e.pageY - containerOffset.top;

    measureStart = {
      x: e.pageX,
      y: e.pageY,
      relX: relX,
      relY: relY,
      containerOffset: containerOffset
    };

    measureLine = $('<svg class="measure-line-svg" style="position:absolute;top:0;left:0;width:100%;height:100%;pointer-events:none;z-index:99;overflow:visible;"><line class="measure-line" stroke="#ff0000" stroke-width="2" stroke-dasharray="4,2"/><circle class="measure-start" r="4" fill="#ff0000"/><circle class="measure-end" r="4" fill="#ff0000"/></svg>');
    container.append(measureLine);

    measureLine.find('.measure-start').attr('cx', relX).attr('cy', relY);
    measureLine.find('.measure-line').attr('x1', relX).attr('y1', relY)
                                     .attr('x2', relX).attr('y2', relY);
    measureLine.find('.measure-end').attr('cx', relX).attr('cy', relY);
  });

  // Measure - mousemove
  $(document).on('mousemove.measure', function(e) {
    if (!measureMode || !measureStart) return;

    var endRelX = e.pageX - measureStart.containerOffset.left;
    var endRelY = e.pageY - measureStart.containerOffset.top;

    measureLine.find('.measure-line').attr('x2', endRelX).attr('y2', endRelY);
    measureLine.find('.measure-end').attr('cx', endRelX).attr('cy', endRelY);

    var dx = e.pageX - measureStart.x;
    var dy = e.pageY - measureStart.y;
    var pixelDist = Math.sqrt(dx*dx + dy*dy);
    var microns = pixelDist / pixelsPerMicron;

    if (!measureLabel) {
      measureLabel = $('<div class="measure-label" style="position:absolute;background:rgba(0,0,0,0.8);color:white;padding:4px 8px;border-radius:4px;font-size:12px;z-index:99;pointer-events:none;"></div>');
      $('.gallery-drag-area').append(measureLabel);
    }

    measureLabel.text(microns.toFixed(1) + ' \u00b5m (' + Math.round(pixelDist) + ' px)');
    measureLabel.css({left: (endRelX + 15) + 'px', top: (endRelY - 10) + 'px'});
  });

  // Measure - mouseup (keep line visible)
  $(document).on('mouseup.measure', function(e) {
    if (!measureMode || !measureStart) return;
    measureStart = null;
  });

  // Click outside image to clear measurement
  $(document).on('click.measure', function(e) {
    if (!measureMode) return;
    if (!$(e.target).closest('.image-card').length &&
        !$(e.target).closest('.measure-label').length &&
        !$(e.target).closest('[id$="measure_toggle"]').length) {
      removeMeasureLine();
    }
  });

  // ============================================================================
  // Selection (client-side visual state management)
  // ============================================================================

  // Mirror of the server-side selection (rv$selected_images). The server
  // pushes every change here via syncSelection; clicks and drag-selects also
  // update it locally so highlights survive a re-render that happens before
  // the server echo arrives.
  var selectedIds = [];

  function applySelection() {
    $('.image-card').each(function() {
      $(this).toggleClass('selected',
                          selectedIds.indexOf($(this).data('img')) >= 0);
    });
  }

  // Single-click selection on image cards (toggle .selected class in JS)
  $(document).on('click', '.image-card', function(e) {
    if (measureMode) return;
    if (wasDragging) {
      wasDragging = false;
      return;
    }
    $(this).toggleClass('selected');
    var imgId = $(this).data('img');
    if (selectedIds.indexOf(imgId) >= 0) {
      selectedIds = selectedIds.filter(function(id) { return id !== imgId; });
    } else {
      selectedIds = selectedIds.concat([imgId]);
    }
    Shiny.setInputValue('gallery-toggle_image',
      {img: imgId, time: new Date().getTime()},
      {priority: 'event'});
  });

  // Sync selection state from server: fires on every rv$selected_images
  // change (clicks echoed back, select-page, deselect-all, and clears done
  // by the validation module after relabel/store-annotations).
  Shiny.addCustomMessageHandler('syncSelection', function(msg) {
    selectedIds = msg.selected || [];
    applySelection();
  });

  // Re-apply highlights after the gallery re-renders: renderUI replaces the
  // DOM, wiping .selected classes even though the images are still selected
  // server-side. shiny:value fires just before the output is swapped in, so
  // defer with setTimeout until the new DOM exists.
  $(document).on('shiny:value', function(e) {
    if (e.name === 'gallery-image_gallery') {
      setTimeout(applySelection, 0);
    }
  });

  // Drag-select
  $(document).on('mousedown', '.gallery-drag-area', function(e) {
    if (measureMode) return;
    if ($(e.target).closest('.image-card').length > 0 &&
        !e.shiftKey) return;

    startX = e.clientX;
    startY = e.clientY;
    wasDragging = false;

    selectionBox = $('<div class="selection-box-active"></div>');
    selectionBox.css({
      position: 'fixed',
      border: '2px dashed #007bff',
      background: 'rgba(0,123,255,0.1)',
      'pointer-events': 'none',
      'z-index': 1000,
      left: startX + 'px',
      top: startY + 'px',
      width: '0px',
      height: '0px'
    });
    $('body').append(selectionBox);

    e.preventDefault();
  });

  $(document).on('mousemove', function(e) {
    if (!selectionBox) return;

    var dx = Math.abs(e.clientX - startX);
    var dy = Math.abs(e.clientY - startY);
    if (dx > 5 || dy > 5) wasDragging = true;

    selectionBox.css({
      left: Math.min(e.clientX, startX) + 'px',
      top: Math.min(e.clientY, startY) + 'px',
      width: Math.abs(e.clientX - startX) + 'px',
      height: Math.abs(e.clientY - startY) + 'px'
    });
  });

  $(document).on('mouseup', function(e) {
    if (!selectionBox) return;

    if (wasDragging) {
      var boxRect = {
        left: Math.min(e.clientX, startX),
        top: Math.min(e.clientY, startY),
        right: Math.max(e.clientX, startX),
        bottom: Math.max(e.clientY, startY)
      };

      var selected = [];
      $('.image-card').each(function() {
        var rect = this.getBoundingClientRect();
        if (rect.left < boxRect.right && rect.right > boxRect.left &&
            rect.top < boxRect.bottom && rect.bottom > boxRect.top) {
          selected.push($(this).data('img'));
          $(this).addClass('selected');
        }
      });

      if (selected.length > 0) {
        selectedIds = selectedIds.concat(selected.filter(function(id) {
          return selectedIds.indexOf(id) < 0;
        }));
        Shiny.setInputValue('gallery-drag_select',
          {images: selected, time: new Date().getTime()},
          {priority: 'event'});
      }
    }

    selectionBox.remove();
    selectionBox = null;
  });
});
