$(document).ready(function(){
  $('input[name=candidates]').on('click', function(event){
    if($('input[name=candidates]:checked').length > 3){
      $(this).prop('checked', false);
    }
  });
  $('input[name=candidates]').on('click', function(event){
    if($('input[name=candidates]:checked').length === 0){
      $(this).prop('checked', true);
    }
  });
});