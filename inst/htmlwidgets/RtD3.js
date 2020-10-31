HTMLWidgets.widget({

  name: 'RtD3',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        console.log(x);

        window.onload = function()
          {
            ReactDOM.render(
                React.createElement(RtD3js.default, {widget: 'summaryWidget', x: x}),
                document.getElementById('root')
            );
          };

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
