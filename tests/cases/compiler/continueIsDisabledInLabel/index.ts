(function () {
    b: {
        if (0) continue b;
        //~^ ERROR: A 'continue' statement can only be used within an enclosing iteration statement.
        (function () {
            b: {
                if (0) continue b;
                //~^ ERROR: A 'continue' statement can only be used within an enclosing iteration statement.
                Number("d");
            }
        }());
    }
}());