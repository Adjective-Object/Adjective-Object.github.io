
var projListElem = document.getElementById('project-list');
var projTitleText = document.getElementById('project-title-text');
var selectedProject = undefined;

function fixSelectedProject() {
  var projElem = document.getElementById('project-' + selectedProject);

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

function selectProject(project) {

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

  // put focus on this element
  if (project != undefined) {
    var projElem = document.getElementById('project-' + project);
    projElem.classList.add('focus');
    relY = projElem.offsetTop;
    relX = projElem.offsetLeft;
    projElem.style.transform = 'translate(-' + relX + 'px, -' + relY + 'px)';
    projListElem.classList.add('focusing')
    
    projTitleText.innerHTML = 'Project :: '+ project
  }
  // clear ofocus on classList
  else {
    projListElem.classList.remove('focusing');
    projTitleText.innerHTML = 'Projects'
  }

  selectedProject = project;
}


// add click handlers for individual links
var projlinks = document.getElementsByClassName('proj-link');
for (var i=0; i<projlinks.length; i++) {
  var elem = projlinks[i];
  elem.addEventListener('click', function(evt) {
    evt.preventDefault();
    selectProject(evt.target.href.split('project-')[1]);
  }, false);
}

// add click handler for clear current project link
document.getElementById('clear-project-link').addEventListener('click',
  function(evt) {
    evt.preventDefault();
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