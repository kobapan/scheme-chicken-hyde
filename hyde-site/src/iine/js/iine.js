// iine.js
$(document).ready(function(){
    $('.vote-button').each(function(){
        var $vote = $(this);
        // 表示処理
        $.ajax({
            type: 'GET',
            url: '/iine/?mode=get',
            cache: false,
            success: function(data){
                $vote.find('.number').text(data);
            }
        });
        // 追加処理
        $vote.click(function(){
            $.ajax({
                type: 'POST',
                url: '/iine/?mode=put',
                cache: false,
                success: function(data){
                    $vote.find('.number').text(data);
                }
            });
            return(false);
        });
    });
});
