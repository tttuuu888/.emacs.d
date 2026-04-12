document.addEventListener('DOMContentLoaded', function() {
  document.querySelectorAll('pre.src').forEach(function(pre) {
    var btn = document.createElement('button');
    btn.className = 'copy-btn';
    btn.textContent = 'Copy';
    btn.addEventListener('click', function() {
      var clone = pre.cloneNode(true);
      clone.querySelectorAll('.copy-btn').forEach(function(b) { b.remove(); });
      navigator.clipboard.writeText(clone.textContent);
      btn.textContent = 'Copied!';
      setTimeout(function() { btn.textContent = 'Copy'; }, 1500);
    });
    pre.appendChild(btn);
  });
});
