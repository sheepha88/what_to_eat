
Shiny.addCustomMessageHandler("showFiveRecs",
    function(x){
        $("#res_his-rec_his_bar div").slice(5).addClass("sr-only");
        // alert("hello" + x);
    }
);


