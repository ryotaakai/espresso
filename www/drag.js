var datasets = {};
var dragOver = function(e) { e.preventDefault(); };
var dropData = function(e) {
    console.log("handledata")
    e.preventDefault();
    handleDrop(e.dataTransfer.files);
};
var handleDrop = function(files) {
    console.log("handleddrop")
    for (var i = 0, f; f = files[i]; i++) {
    var reader = new FileReader();

    reader.onload = (function(file) {
        return function(e) {
        datasets[file.name.toLowerCase()] = e.target.result;
        Shiny.onInputChange("mydata", datasets);
        };
    })(f);
    reader.readAsText(f);
    }
};
// debug
var printData = function(data) {
    var div = document.createElement("div");
    div.innerHTML = datasets[data];
    document.getElementById("data-output").appendChild(div);
};
Raw;

