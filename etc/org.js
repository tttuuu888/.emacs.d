document.addEventListener('DOMContentLoaded', function() {
  document.querySelectorAll('pre.src').forEach(function(pre) {
    var btn = document.createElement('button');
    btn.className = 'copy-btn';
    btn.textContent = 'Copy';
    btn.addEventListener('click', function() {
      navigator.clipboard.writeText(pre.textContent.replace(/^Copy/, ''));
      btn.textContent = 'Copied!';
      setTimeout(function() { btn.textContent = 'Copy'; }, 1500);
    });
    pre.appendChild(btn);
  });
});
