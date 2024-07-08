// custom.js
document.addEventListener("DOMContentLoaded", function() {
  var slides = document.querySelectorAll('.remark-slide-container');
  slides.forEach(function(slide) {
    slide.style.transition = 'transform 0.5s ease';
  });
});
