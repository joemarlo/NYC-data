
// set the dimensions and margins of the graph
var margin = {top: 10, right: 30, bottom: 30, left: 60},
    width = 700 - margin.left - margin.right,
    height = 300 - margin.top - margin.bottom;
    
// Function to parse the date / time
var	parseDate = d3.timeParse("%Y-%m-%d");

// Set the axis ranges
var x = d3.scaleTime().range([ 0, width ]);
var y = d3.scaleLinear().range([ height, 0 ]);

// set the number of y ticks and gridlines
var my_nYticks = 5;

// set the padding around the ticks
var my_tickPadsize = 5;

// set the size of the tick; also controls the grid 'overflow'
var my_tickSize = 15;

// gridlines in x axis function
function make_x_gridlines() {
      return d3.axisBottom(x)
          .ticks();
}

// gridlines in y axis function
function make_y_gridlines() {		
    return d3.axisLeft(y)
        .ticks(my_nYticks);
}

// Define the y axis label
function drawYlabel(selection) {
  selection
    .attr("transform", "rotate(-90)")
    .attr("y", 0 - margin.left)
    .attr("x",0 - (height / 2))
    .attr("dy", "1em")
    .attr("text-anchor", "middle")
    .attr("fill", "#333333")
    .attr("font-size", 14);
}

// Style the plot line
function styleLine(selection) {
  selection
    .attr("fill", "none")
    .attr("stroke", "#2b7551")
    .attr("stroke-width", 2);
}

// function to add "+" in front if positive; add "%"
var printableNumber = function(n) { return (n > 0) ? "+" + n + "%": n + "%"; };

// function to calculat year-over-year different in two numbers for toolip
function YoY_diff(latest_number, old_number) {
  // calculate percent difference
  num = Math.round((latest_number / old_number - 1) * 100);
  // add "+" in front if positive and add % sign
  num = printableNumber(num);
  return num;
}

// Define the tooltip
var Tooltip = d3.select("body")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip")
  .style("background-color", "white")
  .style("color", "#333333")
  .style("border", "solid")
  .style("border-width", "2px")
  .style("border-radius", "0px")
  .style("padding", "5px");

// append the svg object: subway
var svg_S = d3.select("div#subway_citibike")
  .append("div")
  // Container class to make it responsive.
  .classed("svg-container", true) 
  .append("svg")
  // Responsive SVG needs these 2 attributes and no width and height attr.
  .attr("preserveAspectRatio", "xMinYMin meet")
  .attr("viewBox", "0 0 700 300")
  // Class to make it responsive.
  .classed("svg-content-responsive", true)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

// append the svg object: Citibike
var svg_C = d3.select("div#subway_citibike")
  .append("div")
  // Container class to make it responsive.
  .classed("svg-container", true) 
  .append("svg")
  // Responsive SVG needs these 2 attributes and no width and height attr.
  .attr("preserveAspectRatio", "xMinYMin meet")
  .attr("viewBox", "0 0 700 300")
  // Class to make it responsive.
  .classed("svg-content-responsive", true)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");
          
// append the svg object: unemployment 
var svg_unemp = d3.select("div#unemp")
  .append("div")
  // Container class to make it responsive.
  .classed("svg-container", true) 
  .append("svg")
  // Responsive SVG needs these 2 attributes and no width and height attr.
  .attr("preserveAspectRatio", "xMinYMin meet")
  .attr("viewBox", "0 0 700 300")
  // Class to make it responsive.
  .classed("svg-content-responsive", true)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");
          
// append the svg object: flights
var svg_flights = d3.select("div#flights")
  .append("div")
  // Container class to make it responsive.
  .classed("svg-container", true) 
  .append("svg")
  // Responsive SVG needs these 2 attributes and no width and height attr.
  .attr("preserveAspectRatio", "xMinYMin meet")
  .attr("viewBox", "0 0 700 300")
  // Class to make it responsive.
  .classed("svg-content-responsive", true)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");


//Read the subway and citibike data
d3.csv("/d3/covid-impact/data/sub_citi_unemp_flights.csv",

  // When reading the csv, I must format variables:
  function(d){
    return { date : parseDate(d.date), subway_2020 : d.subway_2020, subway_2019 : d.subway_2019, text : d.text, bike_2020 : d.bike_2020, bike_2019 : d.bike_2019, ICSA : d.ICSA, flights : d.flights, ICSA_exists : d.ICSA_exists }
  },

  // Now I can use this dataset:
  function(data) {

  // subway plot
    // Scale the range of the data
    x.domain(d3.extent(data, function(d) { return d.date; })).nice();
    y.domain( [0, 6] );
    
    // add the X gridlines
    svg_S.append("g")			
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(make_x_gridlines()
          .tickSize(-height-my_tickSize)
          .tickFormat("")
      );
      
    // add the Y gridlines
    svg_S.append("g")			
      .attr("class", "grid")
      .call(make_y_gridlines()
          .tickSize(-width-my_tickSize)
          .tickFormat("")
      );
      
    // add the X axis
    svg_S.append("g")
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(d3.axisBottom(x)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize));

    // Add Y axis
    svg_S.append("g")
      .attr("class", "grid")
      .call(d3.axisLeft(y)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nYticks));
      
    // text label for the y axis
    svg_S.append("text").call(drawYlabel).text("Subway ridership (millions)");
    
    // Add 2019 line
    svg_S.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#333333")
      .attr("stroke-width", 1)
      .attr("opacity", 0.5)
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d.subway_2019) })
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.subway_2019 != "NA" })
        );
    
    // Add the 2020 line
    svg_S.append("path")
      .datum(data)
      .call(styleLine)
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d.subway_2020) })
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.subway_2020 != "NA" })
        );
    
    // add label for 2019
    svg_S.append("text")
        .attr("x", (width * 0.95))             
        .attr("y", (height * 0.07))
        .attr("text-anchor", "left")  
        .style("font-size", "12px") 
        .style("font-weight", "700") 
        .style("fill", "#333333")
        .style("opacity", 0.8)
        .text("2019");
    
    // add label for 2019
    svg_S.append("text")
        .attr("x", (width * 0.87))             
        .attr("y", (height * 0.77))
        .attr("text-anchor", "left")  
        .style("font-size", "12px") 
        .style("font-weight", "700") 
        .style("fill", "#2b7551") 
        .text("2020");
        
    // Add the points
    svg_S
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.date) } )
        .attr("cy", function(d) { return y(d.subway_2020) } )
        .attr("class", function(d, i) {return "pt" + i;})
        .attr("r", 4)
        .attr("stroke", "#2b7551")
        .attr("stroke-width", 2)
        .attr("fill", "white")
        // Three function that change the tooltip when user hover / move / leave a cell
        .on('mouseover', function(d, i) {
            console.log("mouseover on", this);
            // make the mouseover'd element big
            d3.selectAll("circle.pt" + i)
              .transition()
              .duration(75)
              .attr('r', 8)
              .attr('fill', '#333333')
              .attr('stroke-opacity', 0);
            
            // this makes the tooltip show
            Tooltip.style("opacity", 1);
          })
          
        .on('mousemove', function(d, i) {
          // this makes the tooltip move
          Tooltip
            .html(
              "<b>" + d.text + "</b><br>" + 
              Math.round(d.subway_2020 * 10, 2) / 10 + " million subway rides (" +
              YoY_diff(d.subway_2020, d.subway_2019) + " vs. 2019) <br>"
              )
            .style("left", d3.event.pageX + 10 + "px")
            .style("top", d3.event.pageY + "px");
        })
        
        .on('mouseout', function(d, i) {
            console.log("mouseout", this);
            // return the mouseover'd element to the original format
            d3.selectAll("circle.pt" + i)
              .transition()
              .duration(200)
              .attr('r', 4)
              .attr('fill', 'white')
              .attr('stroke-opacity', 1);
              
              // this makes the tooltip disappear
              Tooltip.style("opacity", 0);
          });



  // citibike plot
  
    // Scale the range of the data
    x.domain(d3.extent(data, function(d) { return d.date; })).nice();
    y.domain( [0, 100] );
    
    // add the X gridlines
    svg_C.append("g")			
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(make_x_gridlines()
          .tickSize(-height-my_tickSize)
          .tickFormat("")
      );
      
    // add the Y gridlines
    svg_C.append("g")			
      .attr("class", "grid")
      .call(make_y_gridlines()
          .tickSize(-width-my_tickSize)
          .tickFormat("")
      );
      
    // add the X axis
    svg_C.append("g")
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(d3.axisBottom(x)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize));

    // Add Y axis
    svg_C.append("g")
      .attr("class", "grid")
      .call(d3.axisLeft(y)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nYticks));
      
    // text label for the y axis
    svg_C.append("text").call(drawYlabel).text("Citibike ridership (thousands)");

    // Add the 2019 line
    svg_C.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#333333")
      .attr("stroke-width", 1)
      .attr("opacity", 0.5)
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d.bike_2019) })
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.bike_2019 != "NA" })
        );

    // Add the 2020 line
    svg_C.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#3262a8")
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d.bike_2020) })
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.bike_2020 != "NA" })
        );
    
    // Add the points
    var circle_bikes = svg_C
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.date) } )
        .attr("cy", function(d) { return y(d.bike_2020) } )
        .attr("class", function(d, i) {return "pt" + i;})
        //make radius 0 if bike count is NA, otherwise make it 4
        .attr("r", 4)
        .attr("stroke", "#3262a8")
        .attr("stroke-width", 2)
        .attr("fill", "white")
        
        // Three function that change the tooltip when user hover / move / leave a cell
        .on('mouseover', function(d, i) {
            console.log("mouseover on", this);
            // make the mouseover'd element big
            d3.selectAll("circle.pt" + i)
              .transition()
              .duration(75)
              .attr('r', 8)
              .attr('fill', '#333333')
              .attr('stroke-opacity', 0);
            
            // this makes the tooltip show
            Tooltip.style("opacity", 1);
          })
          
        .on('mousemove', function(d, i) {
          // this makes the tooltip move
          Tooltip
            .html(
              "<b>" + d.text + "</b><br>" + 
                  "- " + Math.round(d.subway_2020 * 10, 2) / 10 + " million subway rides (" +
                  YoY_diff(d.subway_2020, d.subway_2019) + " vs. 2019) <br>" + 
                  "- " + Math.round(d.bike_2020 * 10, 2) / 10 + " thousand Citibike rides (" 
                  + YoY_diff(d.bike_2020, d.bike_2019) + ")"
                  )
            .style("left", d3.event.pageX + 10 + "px")
            .style("top", d3.event.pageY + "px");
        })
        
        .on('mouseout', function(d, i) {
            console.log("mouseout", this);
            // return the mouseover'd element to the original format
            d3.selectAll("circle.pt" + i)
              .transition()
              .duration(200)
              .attr('r', 4)
              .attr('fill', 'white')
              .attr('stroke-opacity', 1);
              
              // this makes the tooltip disappear
              Tooltip.style("opacity", 0);
          });
  
  // this removes the 'null' values
  circle_bikes.filter(function(d) { 
	  console.log(d);
	return d.bike_2020 == "NA" }).remove();
  
  
  // unemployment plot
  
    // Scale the range of the data
    x.domain(d3.extent(data, function(d) { return d.date; })).nice();
    y.domain( [0, 7] );
    

    // add the X gridlines
    svg_unemp.append("g")			
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(make_x_gridlines()
          .tickSize(-height-my_tickSize)
          .tickFormat("")
      );
      
    // add the Y gridlines
    svg_unemp.append("g")			
      .attr("class", "grid")
      .call(make_y_gridlines()
          .tickSize(-width-my_tickSize)
          .tickFormat("")
      );
      
    // add the X axis
    svg_unemp.append("g")
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(d3.axisBottom(x)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize));

    // Add Y axis
    svg_unemp.append("g")
      .attr("class", "grid")
      .call(d3.axisLeft(y)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nYticks));
      
    // text label for the y axis
    svg_unemp.append("text").call(drawYlabel).text("Weekly unemployment claims (million)");

    // Add the line
    var line_unemp = svg_unemp.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#2b7551")
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d.ICSA) })
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.ICSA > 0 })
        );
        
    // Add the points
    var circle_unemp = svg_unemp
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.date) } )
        .attr("cy", function(d) { return y(d.ICSA) } )
        .attr("class", function(d, i) {return "pt" + i;})
        .attr("r", 4)
        .attr("stroke", "#2b7551")
        .attr("stroke-width", 2)
        .attr("fill", "white")
        
        // Three function that change the tooltip when user hover / move / leave a cell
        .on('mouseover', function(d, i) {
            console.log("mouseover on", this);
            // make the mouseover'd element big
            d3.selectAll("circle.pt" + i)
              .transition()
              .duration(75)
              .attr('r', 8)
              .attr('fill', '#333333')
              .attr('stroke-opacity', 0);
            
            // this makes the tooltip show
            Tooltip.style("opacity", 1);
          })
          
        .on('mousemove', function(d, i) {
          // this makes the tooltip move
          Tooltip
            .html("<b>" + d.text + "</b><br>" + 
                  "- " + Math.round(d.subway_2020 * 10, 2) / 10 + " million subway rides <br>" + 
                  "- " + Math.round(d.bike_2020 * 10, 2) / 10 + " thousand Citibike rides <br>" +
                  "- " + Math.round(d.ICSA * 10, 2) / 10 + " million unemployment claims <br>" +
                  "- " + Math.round(d.flights * 10, 2) / 10 + " thousand commercial flights")
            .style("left", d3.event.pageX + 10 + "px")
            .style("top", d3.event.pageY + "px");
        })
        
        .on('mouseout', function(d, i) {
            console.log("mouseout", this);
            // return the mouseover'd element to the original format
            d3.selectAll("circle.pt" + i)
              .transition()
              .duration(200)
              .attr('r', 4)
              .attr('fill', 'white')
              .attr('stroke-opacity', 1);
              
              // this makes the tooltip disappear
              Tooltip.style("opacity", 0);
          });
          
  // this removes the circles where we don't have ICSA values
  circle_unemp.filter(function(d) { 
	  console.log(d);
	return d.ICSA_exists != "TRUE"}).remove();
	
          
  // flights plot
  
    // Scale the range of the data
    x.domain(d3.extent(data, function(d) { return d.date; })).nice();
    y.domain( [0, 140] );

    // add the X gridlines
    svg_flights.append("g")			
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(make_x_gridlines()
          .tickSize(-height-my_tickSize)
          .tickFormat("")
      );
      
    // add the Y gridlines
    svg_flights.append("g")			
      .attr("class", "grid")
      .call(make_y_gridlines()
          .tickSize(-width-my_tickSize)
          .tickFormat("")
      );
      
    // add the X axis
    svg_flights.append("g")
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(d3.axisBottom(x)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize));

    // Add Y axis
    svg_flights.append("g")
      .attr("class", "grid")
      .call(d3.axisLeft(y)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nYticks));
      
    // text label for the y axis
    svg_flights.append("text").call(drawYlabel).text("Daily commercial flights (thousands)");

    // Add the line
    svg_flights.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#2b7551")
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d.flights) })
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.flights != "NA"})
        );

    // Add the points
    var circle_flights = svg_flights
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.date) } )
        .attr("cy", function(d) { return y(d.flights) } )
        .attr("class", function(d, i) {return "pt" + i;})
        .attr("r", 4)
        .attr("stroke", "#2b7551")
        .attr("stroke-width", 2)
        .attr("fill", "white")
        
        // Three function that change the tooltip when user hover / move / leave a cell
        .on('mouseover', function(d, i) {
            console.log("mouseover on", this);
            // make the mouseover'd element big
            d3.selectAll("circle.pt" + i)
              .transition()
              .duration(75)
              .attr('r', 8)
              .attr('fill', '#333333')
              .attr('stroke-opacity', 0);
            
            // this makes the tooltip show
            Tooltip.style("opacity", 1);
          })
          
        .on('mousemove', function(d, i) {
          // this makes the tooltip move
          Tooltip
            .html("<b>" + d.text + "</b><br>" + 
                  "- " + Math.round(d.subway_2020 * 10, 2) / 10 + " million subway rides <br>" + 
                  "- " + Math.round(d.bike_2020 * 10, 2) / 10 + " thousand Citibike rides <br>" +
                  "- " + Math.round(d.ICSA * 10, 2) / 10 + " million unemployment claims <br>" +
                  "- " + Math.round(d.flights * 10, 2) / 10 + " thousand commercial flights")
            .style("left", d3.event.pageX + 10 + "px")
            .style("top", d3.event.pageY + "px");
        })
        
        .on('mouseout', function(d, i) {
            console.log("mouseout", this);
            // return the mouseover'd element to the original format
            d3.selectAll("circle.pt" + i)
              .transition()
              .duration(200)
              .attr('r', 4)
              .attr('fill', 'white')
              .attr('stroke-opacity', 1);
              
              // this makes the tooltip disappear
              Tooltip.style("opacity", 0);
          });
          
  // this removes the 'NA' values
  circle_flights.filter(function(d) { 
	  console.log(d);
	return d.flights == "NA"; }).remove();
          
}
);

