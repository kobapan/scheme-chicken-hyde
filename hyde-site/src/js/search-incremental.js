/**
var data = [{title: "", url: "", content: ""}, ...]
を記載した.jsファイルを事前に読み込んでおくこと。

thanks to http://la.ma.la/search.html
*/

/**
$(window).load(function(){は画像の読み込みまで待ってしまうので、readyを使う
*/
$(document).ready(function() {
    var queryString = getQuery();
    $('form.search input[type="search"]')
        .val(queryString);
    $('div[id="searchbox"]')
        .append(
            '<input type="text" id="q" autocomplete="off" placeholder="サイト内を検索">'
            ,' <span id="stat"></span>'
            ,'<div id="navi"></div>'
            ,'<div id="result"></div>');
    $('input[type="text"]#q')
        .val(queryString)
        .focus()
        .keyup(function(e){
            findPosts($(this).val());
        });
    findPosts(queryString);
});

const max = 10;
var result = [];

function getQuery()
{
    var query = "";
    
    // LocationオブジェクトからString.substring(1)で'?'を取り除き、
    // '&'でパラメタ名毎に分割して配列に入れる。
    var parameters = window.location.search.substring(1).split('&');

    // 'q=' 以外は無視
    $.each(parameters, function(i, v) {
        var param = v.split('=');
        var paramName = param[0];
        var paramValue = param[1];
        if(paramName != 'q') return;
        
        // パラメタ値をデコードし、全角空白は'+'に置換
        // 連続する'+'はひとつに纏める
        // '+'を半角スペースにする
        query += decodeURIComponent(paramValue)
            .replace(/　/g, '+')
            .replace(/\++/g, '+')
            .replace(/\+/g, ' ');
    });
    return query;
}

function findPosts(query) {
	if(!query){return false;}
    result = [];
	const reg = new RegExp(query, "g");
    $.each(data, function(i,v) {
		var res = reg.exec(v.title + ' ' + v.content);
		if(!res) return;//eachにおけるcontinue
		if(res.index != -1){
			result.push({title: v.title
                         , url: v.url
                         , title_content: v.title + ' ' + v.content
                         , idx: res.index
                         , len: res[0].length});
		}
    });

	if(result.length){
		$("#stat").html(result.length +"件見つかりました。");
        pageNavi();
        view(0);
	    sw(0);
	    return true;
	} else {
		$("#stat").html("0件見つかりました。");
        return false;
    }
}

function pageNavi(){
	var len = result.length;
	var ct = Math.ceil(len/max);
	var buf = "";
	for(var i=0;i<ct;i++){
		buf += "<span onclick='view(" + i
			+ ");sw(" + i + ")'>" + (i+1) + "</span>";
	}
	$("#navi").html(buf);
}

function view(offset=0){
    offset = offset*max;
    // reverse()が元配列を変えてしまうので、slice()をかます
	var r = result.slice().reverse().slice(offset, (offset+max));
	var buf = "<dl>";
    $.each(r , function(i,v) {
        buf += "\n<dt><a href='" + v.url + "'>"
            + v.title
            + "</a><dd>"
            + snippet(v.title_content,v.idx,v.len);
    });
	$("#result").html(buf);
}

function sw(index){
    $("#navi span").removeAttr("class");
    $("#navi span").eq(index).attr("class" , "selected");
}

function snippet(v,idx,len){
	return v.substring(idx - 20,idx)
		+ "<b>"
		+ v.substr(idx,len)
		+ "</b>"
		+ v.substr(idx+len,50);
}

