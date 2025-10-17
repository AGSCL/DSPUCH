<script>
(function() {
  // Selecciona los elementos con la clase "details-open-code"
  var divHTML = document.querySelectorAll(".details-open-code");
  divHTML.forEach(function (el) {
    var preNodes = el.getElementsByTagName("pre");
    if (preNodes.length > 0) {
      var outputNode = preNodes[0];
      var escapedContent = outputNode.textContent; // Usar textContent para obtener texto plano
      outputNode.textContent = escapedContent;
      outputNode.outerHTML = "<details open class='output'><summary>código</summary><pre><code>" + escapedContent + "</code></pre></details>";
    }
  });
})();
(function() {
  // Selecciona los elementos con la clase "details-code"
  var divHTML = document.querySelectorAll(".details-code");
  divHTML.forEach(function (el) {
    var preNodes = el.getElementsByTagName("pre");
    if (preNodes.length > 0) {
      var outputNode = preNodes[0];
      var escapedContent = outputNode.textContent; // Usar textContent para obtener texto plano
      outputNode.textContent = escapedContent;
      outputNode.outerHTML = "<details class='output'><summary>código</summary><pre><code>" + escapedContent + "</code></pre></details>";
    }
  });
})();
(function() {
  // Selecciona los elementos con la clase "details-open"
  var divHTML = document.querySelectorAll(".details-open");
  divHTML.forEach(function (el) {
    var preNodes = el.getElementsByTagName("pre");
    if (preNodes.length > 1) {
      var outputNode = preNodes[1];
      var escapedContent = outputNode.textContent; // Usar textContent para obtener texto plano
      outputNode.textContent = escapedContent;
      outputNode.outerHTML = "<details open class='output'><summary>salida</summary><pre><code>" + escapedContent + "</code></pre></details>";
    }
  });
})();
(function() {
  // Selecciona los elementos con la clase "details"
  var divHTML = document.querySelectorAll(".details");
  divHTML.forEach(function (el) {
    var preNodes = el.getElementsByTagName("pre");
    if (preNodes.length > 1) {
      var outputNode = preNodes[1];
      var escapedContent = outputNode.textContent; // Usar textContent para obtener texto plano
      outputNode.textContent = escapedContent;
      outputNode.outerHTML = "<details class='output'><summary>salida</summary><pre><code>" + escapedContent + "</code></pre></details>";
    }
  });
})();
</script>
