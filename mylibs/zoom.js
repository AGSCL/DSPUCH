<script src="https://lab.hakim.se/zoom-js/js/zoom.js"></script>
<script>
(function () {
  function attachZoomHandler(slide) {
    const scaler = slide.closest('.remark-slide-container')
                       ?.querySelector('.remark-slide-scaler');
    if (!scaler) return;

    // Evita listeners duplicados
    scaler.removeEventListener('dblclick', handleDoubleClick);
    scaler.addEventListener('dblclick', handleDoubleClick);
  }

  function handleDoubleClick(event) {
    event.preventDefault();
    const scaler = event.currentTarget;
    const rect = scaler.getBoundingClientRect();

    zoom.to({
      x: event.clientX,
      y: event.clientY,
      width: rect.width * 0.6,
      height: rect.height * 0.6
    });
  }

  slideshow.on('afterShowSlide', slide => {
    const visible = document.querySelector('.remark-visible');
    if (visible) attachZoomHandler(visible);
  });

  slideshow.on('beforeHideSlide', () => {
    zoom.out();
  });

  // Por si recargas la página dentro de una slide ya visible
  document.addEventListener('DOMContentLoaded', () => {
    const visible = document.querySelector('.remark-visible');
    if (visible) attachZoomHandler(visible);
  });
})();
</script>
