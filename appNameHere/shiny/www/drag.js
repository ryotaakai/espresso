var datasets = {};
var timeoutId;
var dragOver = function(e) { 
    e.preventDefault();
    console.log("dragdata");
    clearTimeout( timeoutId ) ;

	document.getElementById("drop-area").style.backgroundColor = "#fff0c7";

	timeoutId = setTimeout( function () {
		document.getElementById("drop-area").style.backgroundColor = "#faf9f8";
	}, 100 ) ;
    

};
var dropData = function(e) {
    console.log("handledata");
    e.preventDefault();
    console.log(e.dataTransfer.files);
    handleDrop(e.dataTransfer.files);
};
var handleDrop = function(files) {
    console.log("handleddrop");
    var reader = new FileReader();
    reader.onload = (function(file) {
        return function(e) {
        datasets[file.name.toLowerCase()] = e.target.result;
        Shiny.onInputChange("dataDrop", datasets);
        fileResult = reader.result.split('\r\n');
        };
    })(files[0]);
    reader.readAsText(files[0]);
};
// debug
var printData = function(data) {
    var div = document.createElement("div");
    div.innerHTML = datasets[data];
    document.getElementById("data-output").appendChild(div);
};