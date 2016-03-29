
var projListElem = document.getElementById('project-list');
var projTitleText = document.getElementById('project-title-text');

var selectedProject = undefined;

function fixSelectedProject() {
  var projElem = document.getElementById('proj-' + selectedProject);

  relY = projElem.offsetTop;
  relX = projElem.offsetLeft;

  projElem.style.transition = '';
  projElem.style.transform = 'translate(-' + relX + 'px, -' + relY + 'px)';
  projElem.style.transition = undefined;
}


function zindex(p) {
  p.style.zIndex = 1000;
  setTimeout(function(){
    p.style.zIndex = ''; 
  }, 200)
}

function selectProject(project, fast) {

  console.log('selecting project ' + project)
  if (selectedProject == project) {
    return;
  }

  // remove focus from existing focused elements
  var cur_focus = document.getElementsByClassName('focus');
  for (var i=0; i<cur_focus.length; i++) {
    var p = cur_focus[i];
    p.classList.remove('focus')

    zindex(p);
  }


  // clear ofocus on classList
  if (project == undefined) {
    projListElem.classList.remove('focusing');
    projTitleText.innerHTML = 'Projects'
  }

  // put focus on this element
  else {
    var projElem = document.getElementById('proj-' + project);
    if (fast) {
      projListElem.classList.add('notrans');
      setTimeout(function() {
        projListElem.classList.remove('notrans')
      }, 200);
    }

    projElem.classList.add('focus');
    relY = projElem.offsetTop;
    relX = projElem.offsetLeft;
    projElem.style.transform = 'translate(-' + relX + 'px, -' + relY + 'px)';
    projListElem.classList.add('focusing')
    
    projTitleText.innerHTML = 'Project: '+ project
  }

  selectedProject = project;
}


// add click handlers for individual links
var projlinks = document.getElementsByClassName('proj-link');
for (var i=0; i<projlinks.length; i++) {
  var elem = projlinks[i];
  elem.addEventListener('click', function(evt) {
    evt.preventDefault();
    window.location.hash = evt.target.href.split("#")[1];
    selectProject(evt.target.href.split('project-')[1]);
  }, false);
}

// add click handler for clear current project link
document.getElementById('clear-project-link').addEventListener('click',
  function(evt) {
    evt.preventDefault()
    if(history.pushState) {
      history.pushState(null, null, '#projects')
    }
    selectProject(undefined);
  });


// add handler for project list resize
window
  .addEventListener('resize',
    function(evt) {
      if(selectedProject != undefined) {
        fixSelectedProject();
      }
    });

// onload capture
if (window.location.hash.startsWith("#project-")) {
  console.log("hash", window.location.hash);
  var projname = window.location.hash.split("-")[1];
  window.scrollTo(0, document.getElementById("projects").offsetTop);
  selectProject(window.location.hash.substring(9), true);
}

