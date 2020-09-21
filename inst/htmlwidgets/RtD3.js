HTMLWidgets.widget({

  name: 'RtD3',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        var vis = new rtVis(x, 100, 100);

        vis.summaryWidget(el)

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
