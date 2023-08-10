/* elm-pkg-js
port copy_to_clipboard_to_js : String -> Cmd msg
*/

// Copied from https://github.com/supermario/elm-pkg-js/blob/master/examples/copy-to-clipboard/src/copy-to-clipboard.js
function init(app) {
    app.ports.copy_to_clipboard_to_js.subscribe(async function(text) {
      copyTextToClipboard(text)
    })
  }
  
  function copyTextToClipboard(text) {
    if (!navigator.clipboard) {
      fallbackCopyTextToClipboard(text);
      return;
    }
    navigator.clipboard.writeText(text).then(function() {
      // console.log('Async: Copying to clipboard was successful!');
    }, function(err) {
      console.error('Error: Could not copy text: ', err);
    });
  }
  
  function fallbackCopyTextToClipboard(text) {
    var textArea = document.createElement("textarea");
    textArea.value = text;
  
    // Avoid scrolling to bottom
    textArea.style.top = "0";
    textArea.style.left = "0";
    textArea.style.position = "fixed";
  
    document.body.appendChild(textArea);
    textArea.focus();
    textArea.select();
  
    try {
      var successful = document.execCommand('copy');
      if (successful !== true) {
        console.log('Error: Copying text command was unsuccessful');
      }
    } catch (err) {
      console.error('Error: Oops, unable to copy', err);
    }
  
    document.body.removeChild(textArea);
  }

exports.init = init