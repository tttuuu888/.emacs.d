document.addEventListener('DOMContentLoaded', () => {
  document.querySelectorAll('pre.src').forEach(pre => {
    const code = pre.textContent;
    const btn = pre.appendChild(document.createElement('button'));
    btn.className = 'copy-btn';
    btn.textContent = 'Copy';
    btn.onclick = () => {
      navigator.clipboard.writeText(code);
      btn.textContent = 'Copied!';
      setTimeout(() => btn.textContent = 'Copy', 1500);
    };
  });
});
