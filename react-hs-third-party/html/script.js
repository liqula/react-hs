



function loadImage () {

    Caman("#my-canvas", "http://localhost:8000/png/test.png", function () {
	console.log(this);

	this.resize({
	    width: 400,
	    height: 400
	});

	this.render();
	
    });
}
