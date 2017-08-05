



function loadImage () {

    Caman("#my-canvas", "http://localhost:8000/png/test.png", function () {
	console.log(this);


	// This resise together with the following render
	// will cause caman.js to replace the canvas with another canvas.
	
	this.resize({
	    width: 400,
	    height: 400
	});

	this.render();
	
    });
}
