function copyToClipBoard(id) {
  var copyText = document.getElementById(id);
  copyText.select();
  copyText.setSelectionRange(0, 99999)
  document.execCommand("copy");
}

document.addEventListener("DOMContentLoaded", function(){
  setTimeout(function(){
    document.querySelector('.back').classList.add('hide');
  }, 1000)
});

Shiny.addCustomMessageHandler('flip-card', function(opts) {
  var card = document.querySelector('.flip-container');
  card.classList.toggle('hover');

  var front = document.querySelector('.front'),
      back = document.querySelector('.back'),
      flippy = document.querySelector('#flippy'),
      btn = document.querySelector("#europe-flip");

  var is_back = card.classList.contains('hover');

  if(is_back){
    // show hide
    back.classList.remove('hide');
    front.classList.add('hide');

    //resize
    var map = HTMLWidgets.find("#europe-counties_map");
    var bars = HTMLWidgets.find("#europe-counties_bars");
    map.resize();
    bars.resize();

    // change button
    btn.innerHTML = "<i class='fa fa-search-minus'></i> Back to Europe"

    // resize
    if(flippy.offsetWidth <= 768)
      flippy.style.height = "185vh";
    else
    flippy.style.height = "95vh";
  } else {
    btn.innerHTML = "<i class='fa fa-search-plus'></i> Focus on Country"
    front.classList.remove('hide');
    back.classList.add('hide');
    flippy.style.height = "95vh";
  }
});
