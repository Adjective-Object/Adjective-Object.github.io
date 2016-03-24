var curanim = undefined;

function ease(t) {
  return (1 - Math.cos(t * 3.14159)) / 2;
}

function scrollPage(elem, time) {
    var start = new Date().getTime();

    var initialScrollX = window.scrollX;
    var initialScrollY = window.scrollY;

    var scrollTargetX = initialScrollX;
    var scrollTargetY = Math.min(elem.offsetTop,
      document.body.scrollHeight - window.innerHeight);

var stepFn = function() {
  var t_raw = Math.min(1,(new Date().getTime()-start)/time);
  var t = ease(t_raw);

  window.scrollTo(
    initialScrollX + (scrollTargetX - initialScrollX) * t,
    initialScrollY + (scrollTargetY - initialScrollY) * t);

  if( t_raw != 1 && window.scrollY != scrollTargetY) {
        window.requestAnimationFrame(stepFn);
  }
};
if(curanim) {
      window.cancelAnimationFrame(curanim) 
    }
    curanim = window.requestAnimationFrame(stepFn);
}



var navLinks = document.getElementsByClassName('nav-link');
for (var i=0; i<navLinks.length; i++) {
  var elem = navLinks[i];
  console.log(elem.getAttribute('data-scroll-time'))
  elem.addEventListener('click', function(evt) {
    scrollPage(
      document.getElementById(
        evt.target.href.split('#')[1]),
        parseInt(evt.target.getAttribute('data-scroll-time')));
  }, false);
}
