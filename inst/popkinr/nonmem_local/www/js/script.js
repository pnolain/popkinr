Shiny.addCustomMessageHandler('show_waiting_cursor',  function(id) {
  $('#' + id).addClass('waiting');
});

Shiny.addCustomMessageHandler('hide_waiting_cursor',  function(id) {
  $('#' + id).removeClass('waiting');
});


function show_run_details(msg){
  var number = Math.random();

  var objects = {
    rnd: number,
    run_number: msg
  };

  Shiny.onInputChange('show_run_details', JSON.stringify(objects));
}

function kill_run(msg){

swal({
  title: "Are you sure?",
  text: "Killing a run will kill the NONMEM process of the run.",
  type: "warning",
  showCancelButton: true,
  confirmButtonText: 'Yes'
})
.then((result) => {
  if (result.value) {
    swal(
      'Done!',
      'Run process will be killed.',
      'success'
    );

    var number = Math.random();

    var objects = {
      rnd: number,
      run_number: msg
    };

    Shiny.onInputChange('kill_run', JSON.stringify(objects));

  }
});
}

function stop_run(msg){

swal({
  title: "Are you sure?",
  text: "Stopping a run will send a stop signal to NONMEM (i.e. create a sig.stop file in the NONMEM process working directory).",
  type: "warning",
  showCancelButton: true,
  confirmButtonText: 'Yes'
})
.then((result) => {
  if (result.value) {
    swal(
      'Done!',
      'Run will stop.',
      'success'
    );

    var number = Math.random();

    var objects = {
      rnd: number,
      run_number: msg
    };

    Shiny.onInputChange('stop_run', JSON.stringify(objects));

  }
});
}

function next_estimation(msg){

swal({
  title: "Are you sure?",
  text: "The run will move on to next estimation mode or next estimation (i.e. create a sig.next file in the NONMEM process working directory).",
  type: "warning",
  showCancelButton: true,
  confirmButtonText: 'Yes'
})
.then((result) => {
  if (result.value) {
    swal(
      'Done!',
      'Run will move on to next estimation mode or next estimation.',
      'success'
    );

    var number = Math.random();

    var objects = {
      rnd: number,
      run_number: msg
    };

    Shiny.onInputChange('next_estimation', JSON.stringify(objects));

  }
});
}

function show_cs_details(msg){
  var number = Math.random();

  var objects = {
    rnd: number,
    cs_id: msg
  };

  Shiny.onInputChange('show_cs_details', JSON.stringify(objects));
}

function remove_cs(msg){
  var number = Math.random();

  var objects = {
    rnd: number,
    cs_id: msg
  };

  Shiny.onInputChange('remove_cs', JSON.stringify(objects));
}


function kill_all_runs(){

swal({
  title: "Are you sure?",
  text: "You are about to kill all the running NONMEM jobs.",
  type: "warning",
  showCancelButton: true,
  confirmButtonText: 'Yes'
})
.then((result) => {
  if (result.value) {
    swal(
      'Done!',
      'Run processes will be killed.',
      'success'
    );

    var number = Math.random();

    var objects = {
      rnd: number
    };

    Shiny.onInputChange('kill_all_runs', JSON.stringify(objects));

  }
});
}
