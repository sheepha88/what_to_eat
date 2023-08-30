
$('#auth-auth-mod .panel').removeClass('panel-primary');
$('#auth-auth-mod .panel').addClass('panel-border');
$('#auth-auth-mod .panel-body br').remove();

$('#auth-user_id-label').remove();
$('#auth-user_id').attr("placeholder", "이메일을 입력하세요.");

$('#auth-user_pwd-label').remove();
$('#auth-user_pwd').attr("placeholder", "비밀번호를 입력하세요.");


$('#auth-go_auth').css('margin-top', '1rem');
$('#auth-go_auth').css('margin-bottom', '1.5rem');

$('#auth-go_auth').after(
	'<div class="row"> \
		<div class="col-xs-12"> \
            <a id="create_account" href="#" class="action-button account-link" data-toggle="modal" data-target="#signup_modal">회원가입 신청</a> \
        </div> \
	</div>'
);

$('#auth-result_auth').css('margin-top', '1rem');

$('#signup_modal').on('shown.bs.modal', function () {
    $('#user_name').focus();
}) 

$('#signup_modal').on('hide.bs.modal', function () {
    Shiny.setInputValue("signup_close_1", 1, {priority: "event"});
}) 

$(document).keypress(function(e) {
    if ( $("#signup_modal").hasClass('in') && (e.key == "Enter") )  {
        $('#signup_submit').trigger("click");
    }
});




