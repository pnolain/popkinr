
Shiny.addCustomMessageHandler('scroll_to_item',  function(id) {
  var elem = document.getElementById(id);
  if(elem !== null)
    elem.scrollIntoView();
});


Shiny.addCustomMessageHandler('collapse_box',  function(boxid) {
  $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
});

Shiny.addCustomMessageHandler('show_waiting_cursor',  function(id) {
  $('#' + id).addClass('waiting');
});

Shiny.addCustomMessageHandler('hide_waiting_cursor',  function(id) {
  $('#' + id).removeClass('waiting');
});


Shiny.addCustomMessageHandler('popup_msg',  function(msg) {
  var title = 'Message';
  var description = '';
  var txt = '';
  var pop_type = String(msg.type);


  if(msg.title){
    title = String(msg.title);
  }

  if(msg.description){
    description = String(msg.description);
  }

  if(msg.detail){
    txt = '<pre>' + msg.detail + '</pre>';
  }

    swal({
      title: title,
      type: pop_type,
      html:
      description +
      txt
    });
});



function compare_control_stream(msg){
  var number = Math.random();

  var objects = {
    rnd: number,
    run_index: msg
  };

  Shiny.onInputChange('click_compare_control_stream', JSON.stringify(objects));
}

function open_run(msg){
  var number = Math.random();

  var objects = {
    rnd: number,
    run_index: msg
  };

  Shiny.onInputChange('click_open_run_to_compare', JSON.stringify(objects));
}

function remove_run(msg){
  var number = Math.random();

  var objects = {
    rnd: number,
    run_index: msg
  };

  Shiny.onInputChange('click_remove_run_from_list', JSON.stringify(objects));
}
