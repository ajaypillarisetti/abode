$(document).ready(function() {
	$("ul.treeview-menu li").click(function(){
		$('table.table.table-striped.table-hover.dataTable.no-footer').delay(1000).attr('style', function(i, style)
			{return style.replace(/width[^;]+;?/g, '');
		});
	});
	$("a#scale_pre").click(function(){
		var value1 = $("#adultPrePM").val();
		var value_updated = value1 * 0.70;
		$('#adultPrePMSD').val(value_updated).toFixed();
	});
	$("a#scale_post").click(function(){
		var value1 = $("#adultPostPM").val();
		var value_updated = value1 * 0.70;
		$('#adultPostPMSD').val(value_updated).toFixed();
	});
});    
