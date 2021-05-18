// iine.js
$(document).ready(function(){
    $('.iine-button').each(function(){
        var $iine = $(this);
        // 表示処理
        $.ajax({
            type: 'GET',
            url: '/iine/?mode=get',
            cache: false,
            success: function(data){
                $iine.find('.number').text(data);
            }
        });
        // 追加処理
        $iine.click(function(){
            $.ajax({
                type: 'POST',
                url: '/iine/?mode=put',
                cache: false,
                success: function(data){
                    $iine.find('.number').text(data);
                }
            });
            return(false);
        });
    });
});
