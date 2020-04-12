
// set the dimensions and margins of the graph
var margin = {top: 10, right: 50, bottom: 40, left: 75},
    width = 700 - margin.left - margin.right,
    height = 300 - margin.top - margin.bottom;
    
// Function to parse the date / time
var	parseDate = d3.timeParse("%Y-%m-%d");

// set opacity to de-emphasize lines and circles when using tooltip
var de_emphasize_opacity = 0.2;

// set death of first death to draw vertical bar
var first_death = parseDate('2020-02-29');

// style the line to be drawn for first death
function styleDeath(selection){
    selection
      .attr("class", "death_bar")
      .attr("x1", x(first_death)) 
      .attr("y1", 0)
      .attr("x2", x(first_death))
      .attr("y2", height)
      .style("stroke-width", 1.5)
      .style("stroke-dasharray", ("4, 4"))
      .style("stroke", "#333333")
      .style("fill", "none");
}

// style the line to be drawn to connect 2020-2019 points
function styleConn(selection){
    selection
        .style('stroke', '#737373')
        .attr('opacity', 0)
        .style("stroke-width", 2.5)
        .style("stroke-dasharray", ("6, 4"));
}

// Set the axis ranges
var x = d3.scaleTime().range([ 0, width ]);
var y = d3.scaleLinear().range([ height, 0 ]);

// set the number of and y ticks and gridlines
var my_nXticks = d3.timeMonth.every(1);
var my_nYticks = 5;

// set the padding around the ticks
var my_tickPadsize = 5;

// set the size of the tick; also controls the grid 'overflow'
var my_tickSize = 15;

// gridlines in x axis function
function make_x_gridlines() {
      return d3.axisBottom(x)
          .ticks(my_nXticks);
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
function printableNumber(n) { return (n > 0) ? "+" + n + "%": n + "%"; }

// function to calculat year-over-year different in two numbers for toolip
function YoY_diff(latest_number, old_number) {
  // calculate percent difference
  num = Math.round((latest_number / old_number - 1) * 100);
  
  if (isNaN(num)){
    return num = '';
  } else {
    // add "+" in front if positive and add % sign
    num = "(" + printableNumber(num) + " vs. 2019)";
  }
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
  
// append the svg object: google trends
var svg_trends = d3.select("div#trends")
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

// append the svg object: 311
var svg_311 = d3.select("div#threeOneOne")
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
    return { date : parseDate(d.date), subway_2020 : d.subway_2020, subway_2019 : d.subway_2019, text : d.text, bike_2020 : d.bike_2020, bike_2019 : d.bike_2019, ICSA_2020 : d.ICSA_2020, ICSA_2019 : d.ICSA_2019, flights : d.flights, ICSA_20_exists : d.ICSA_20_exists, ICSA_19_exists : d.ICSA_19_exists, retail_rec : d.retail_rec, groc_pharm : d.groc_pharm, parks : d.parks, transit : d.transit, workplaces : d.workplaces, Residential : d.Residential }
  },

  // Now I can use this dataset:
  function(data) {
  
  // subway plot
    // Scale the range of the data
    x.domain(d3.extent(data, function(d) { return d.date; }));
    y.domain( [0, 7] );
    
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
        .tickSize(my_tickSize)
        .ticks(my_nXticks)
        .tickFormat(d3.timeFormat("%b")));

    // Add Y axis
    svg_S.append("g")
      .attr("class", "grid")
      .call(d3.axisLeft(y)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nYticks));
      
    // text label for the y axis
    svg_S.append("text").call(drawYlabel).text("Subway ridership (millions)");
    
    // add vertical bar for first death
    svg_S.append("line")
      .call(styleDeath);
      
    // add line between 2019 and 2020 points
    /*
    var conn_lines = svg_S.selectAll("line")
      .data(data)
      .enter()
      .append("line")
        .call(styleConn)
        .attr('opacity', 0)
        .attr("class", function(d, i) {return "line_conn line_conn" + i;})
        .attr("x1", function(d) { return x(d.date) } )
        .attr("y1", function(d) { return y(d.subway_2019) } )
        .attr("x2", function(d) { return x(d.date) } )
        .attr("y2", function(d) { return y(d.subway_2020) } );
    */
      
    // Add 2019 line
    svg_S.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#333333")
      .attr("stroke-width", 1)
      .attr("opacity", 0.5)
      .attr("class", "path_subway path_subway19")
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d.subway_2019) })
        .curve(d3.curveMonotoneX)
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.subway_2019 != "NA" })
        );
    
    // Add the 2020 line
    svg_S.append("path")
      .datum(data)
      .call(styleLine)
      .attr("class", "path_subway")
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d.subway_2020) })
        .curve(d3.curveMonotoneX)
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.subway_2020 != "NA" })
        );
    
        
    // add label for first death
    svg_S.append("text")
        .attr("class", "plot_label")
        .attr("x", (width * 0.505))             
        .attr("y", (height * 0.92))
        .attr("text-anchor", "right")  
        .style("font-size", "12px") 
        .style("font-weight", "700") 
        .style("fill", "#333333")
        .text("First US death");
      svg_S.append("text")
        .attr("class", "plot_label")
        .attr("x", (width * 0.575))             
        .attr("y", (height * 0.98))
        .attr("text-anchor", "right")  
        .style("font-size", "12px") 
        .style("font-weight", "700") 
        .style("fill", "#333333")
        .text("Feb 29");
    
    // add label for 2019
    svg_S.append("text")
        .attr("class", "plot_label")
        .attr("x", (width * 0.94))             
        .attr("y", (height * 0.09))
        .attr("text-anchor", "left")  
        .style("font-size", "12px") 
        .style("font-weight", "700") 
        .style("fill", "#333333")
        .text("2019");
    
    // add label for 2020
    svg_S.append("text")
        .attr("class", "plot_label")
        .attr("x", (width * 0.90))             
        .attr("y", (height * 0.78))
        .attr("text-anchor", "left")  
        .style("font-size", "12px") 
        .style("font-weight", "700") 
        .style("fill", "#2b7551") 
        .text("2020");
        
    // Add the 2019 points
    var circle_subway19 = svg_S
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.date) } )
        .attr("cy", function(d) { return y(d.subway_2019) } )
        .attr("class", function(d, i) {return "pt_subway19" + i;});

    // Add the 2020 points
    svg_S
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.date) } )
        .attr("cy", function(d) { return y(d.subway_2020) } )
        // assign two classes 'pt_subway20' and 'pt_subway20i'
        .attr("class", function(d, i) {return "pt_subway20 pt_subway20" + i;})
        .attr("r", 4)
        .attr("stroke", "#2b7551")
        .attr("stroke-width", 2)
        .attr("fill", "white")
        // Three function that change the tooltip when user hover / move / leave a cell
        .on('mouseover', function(d, i) {
            console.log("mouseover on", this);
              
            // de-emphasize lines, circles, text labels, 
            d3.selectAll("path.path_subway")
              .attr('opacity', de_emphasize_opacity);
            d3.selectAll("circle.pt_subway20")
              .attr('stroke-opacity', de_emphasize_opacity);
            svg_S.selectAll("text.plot_label")
              .attr('opacity', de_emphasize_opacity);
            svg_S.selectAll("line.death_bar")
              .attr('opacity', de_emphasize_opacity);
              
            // make the mouseover'd element big
            d3.selectAll("circle.pt_subway20" + i)
              .transition()
              .duration(75)
              .attr('r', 8)
              .attr('fill', '#333333')
              .attr('stroke-opacity', 0)
              .attr('opacity', 1);
            
            // highlight the 2019 point
            d3.selectAll("circle.pt_subway19" + i)
              .transition()
              .duration(75)
              .attr('r', 6)
              .attr('fill', '#737373')
              .attr('stroke-opacity', 0)
              .attr('opacity', 1);
              
            // highlight the selected points connection
            d3.selectAll("line.line_conn" + i)
              .attr('opacity', 1);
            
            // this makes the tooltip show
            Tooltip.style("opacity", 1);
          })
          
        .on('mousemove', function(d, i) {
          // this makes the tooltip move
          Tooltip
            .html(
              "<b>" + d.text + "</b><br>" + 
              Math.round(d.subway_2020 * 10, 2) / 10 + " million subway rides " +
              YoY_diff(d.subway_2020, d.subway_2019)
              )
            .style("left", d3.event.pageX + 10 + "px")
            .style("top", d3.event.pageY + "px");
        })
        
        .on('mouseout', function(d, i) {
            console.log("mouseout", this);
            
            // return the mouseover'd element to the original format
            d3.selectAll("circle.pt_subway20" + i)
              .transition()
              .duration(200)
              .attr('r', 4)
              .attr('fill', 'white')
              .attr('stroke-opacity', 1)
              .attr('opacity', 1);
            
            // return the 2019 dot
            d3.selectAll("circle.pt_subway19" + i)
              .transition()
              .duration(200)
              .attr('opacity', 0);
              
            // re-emphasis lines and circles
            d3.selectAll("path.path_subway")
              .attr('opacity', 1);
            d3.selectAll("path.path_subway19")
              .attr('opacity', 0.5);
            d3.selectAll("circle.pt_subway20")
              .attr('stroke-opacity', 1);
            svg_S.selectAll("text.plot_label")
              .attr('opacity', 1);
            svg_S.selectAll("line.death_bar")
              .attr('opacity', 1);

            // remove the selected points connection
            d3.selectAll("line.line_conn" + i)
              .attr('opacity', 0);

              // this makes the tooltip disappear
              Tooltip.style("opacity", 0);
          });
  
  // this removes the 'null' values for the 2019 subway circles
  circle_subway19.filter(function(d) { 
	  console.log(d);
	return d.subway_2019 == "NA" }).remove();

/*
  conn_lines.filter(function(d) { 
	  console.log(d);
	return d.subway_2019 == "NA" }).remove();
*/

  // citibike plot
  
    // Scale the range of the data
    x.domain(d3.extent(data, function(d) { return d.date; }));
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
        .tickSize(my_tickSize)
        .ticks(my_nXticks)
        .tickFormat(d3.timeFormat("%b")));

    // Add Y axis
    svg_C.append("g")
      .attr("class", "grid")
      .call(d3.axisLeft(y)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nYticks));
      
    // text label for the y axis
    svg_C.append("text").call(drawYlabel).text("Citibike ridership (thousands)");

    // add vertical bar for first death
    svg_C.append("line")
      .call(styleDeath);

      
    // add line between 2019 and 2020 points
    /*
    var conn_bike_lines = svg_C.selectAll("line")
      .data(data)
      .enter()
      .append("line")
        .call(styleConn)
        .attr("class", function(d, i) {return "line_bike_conn line_bike_conn" + i;})
        .attr("x1", function(d) { return x(d.date) } )
        .attr("y1", function(d) { return y(d.bike_2019) } )
        .attr("x2", function(d) { return x(d.date) } )
        .attr("y2", function(d) { return y(d.bike_2020) } );
    */

    // Add the 2019 line
    svg_C.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#333333")
      .attr("stroke-width", 1)
      .attr("opacity", 0.5)
      .attr("class", "path_bike path_bike19")
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d.bike_2019) })
        .curve(d3.curveMonotoneX)
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.bike_2019 != "NA" })
        );

    // Add the 2020 line
    svg_C.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#3262a8")
      .attr("class", "path_bike")
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d.bike_2020) })
        .curve(d3.curveMonotoneX)
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.bike_2020 != "NA" })
        );
        
    // Add the 2019 points
    svg_C
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.date) } )
        .attr("cy", function(d) { return y(d.bike_2019) } )
        .attr("class", function(d, i) {return "pt_bike19" + i;});
        
    // Add the points
    var circle_bikes = svg_C
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.date) } )
        .attr("cy", function(d) { return y(d.bike_2020) } )
        .attr("class", function(d, i) {return "pt_bike20 pt_bike20" + i;})
        //make radius 0 if bike count is NA, otherwise make it 4
        .attr("r", 4)
        .attr("stroke", "#3262a8")
        .attr("stroke-width", 2)
        .attr("fill", "white")
        
        // Three function that change the tooltip when user hover / move / leave a cell
        .on('mouseover', function(d, i) {
            console.log("mouseover on", this);
            
            // de-emphasis lines and circles
            d3.selectAll("path.path_bike")
              .attr('opacity', de_emphasize_opacity);
            d3.selectAll("circle.pt_bike20")
              .attr('stroke-opacity', de_emphasize_opacity);
            svg_C.selectAll("line.death_bar")
              .attr('opacity', de_emphasize_opacity);
            
            // make the mouseover'd element big
            d3.selectAll("circle.pt_bike20" + i)
              .transition()
              .duration(75)
              .attr('r', 8)
              .attr('fill', '#333333')
              .attr('stroke-opacity', 0);
          
          // highlight the 2019 point
            d3.selectAll("circle.pt_bike19" + i)
              .transition()
              .duration(75)
              .attr('r', 6)
              .attr('fill', '#737373')
              .attr('stroke-opacity', 0)
              .attr('opacity', 1);
            
          // highlight the selected points connection
            d3.selectAll("line.line_bike_conn" + i)
              .attr('opacity', 1);
            
            // this makes the tooltip show
            Tooltip.style("opacity", 1);
          })
          
        .on('mousemove', function(d, i) {
          // this makes the tooltip move
          Tooltip
            .html(
              "<b>" + d.text + "</b><br>" + 
                  Math.round(d.bike_2020 * 10, 2) / 10 + " thousand Citibike rides " 
                  + YoY_diff(d.bike_2020, d.bike_2019)
                  )
            .style("left", d3.event.pageX + 10 + "px")
            .style("top", d3.event.pageY + "px");
        })
        
        .on('mouseout', function(d, i) {
            console.log("mouseout", this);
            // return the mouseover'd element to the original format
            d3.selectAll("circle.pt_bike20" + i)
              .transition()
              .duration(200)
              .attr('r', 4)
              .attr('fill', 'white')
              .attr('stroke-opacity', 1);
              
            // return the 2019 dot
            d3.selectAll("circle.pt_bike19" + i)
              .transition()
              .duration(200)
              .attr('opacity', 0);
              
            // re-emphasis lines and circles
            d3.selectAll("path.path_bike")
              .attr('opacity', 1);
            d3.selectAll("path.path_bike19")
              .attr('opacity', 0.5);
            d3.selectAll("circle.pt_bike20")
              .attr('stroke-opacity', 1);
            svg_C.selectAll("line.death_bar")
              .attr('opacity', 1);
              
            // remove the selected points connection
            d3.selectAll("line.line_bike_conn" + i)
              .attr('opacity', 0);
              
              // this makes the tooltip disappear
              Tooltip.style("opacity", 0);
          });
  
  // this removes the 'null' values
  circle_bikes.filter(function(d) { 
	  console.log(d);
	return d.bike_2020 == "NA" }).remove();
  
  /*
  conn_bike_lines.filter(function(d) { 
	  console.log(d);
	return d.bike_2020 == "NA" || d.bike_2019 == "NA" }).remove();
  */
  
  // unemployment plot
  
    // Scale the range of the data
    x.domain(d3.extent(data, function(d) { return d.date; }));
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
        .tickSize(my_tickSize)
        .ticks(my_nXticks)
        .tickFormat(d3.timeFormat("%b")));

    // Add Y axis
    svg_unemp.append("g")
      .attr("class", "grid")
      .call(d3.axisLeft(y)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nYticks));
      
    // text label for the y axis
    svg_unemp.append("text").call(drawYlabel).text("Weekly unemployment claims (million)");

    // add vertical bar for first death
    svg_unemp.append("line")
      .call(styleDeath);

    // Add the 2019 line 
    svg_unemp.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#333333")
      .attr("stroke-width", 1)
      .attr("opacity", 0.5)
      .attr("class", "path_unemp path_unemp19")
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d.ICSA_2019) })
        .curve(d3.curveMonotoneX)
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.ICSA_2019 != "NA"})
        );

    // Add the line
    var line_unemp = svg_unemp.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#2b7551")
      .attr("class", "path_unemp path_unemp20")
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d.ICSA_2020) })
        .curve(d3.curveMonotoneX)
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.ICSA_2020 > 0 })
        );

    // Add the 2019 points
    svg_unemp
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.date) } )
        .attr("cy", function(d) { return y(d.ICSA_2019) } )
        .attr("class", function(d, i) {return "pt_unemp19" + i;});

    // Add the points
    var circle_unemp = svg_unemp
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.date) } )
        .attr("cy", function(d) { return y(d.ICSA_2020) } )
        .attr("class", function(d, i) {return "pt_unemp20 pt_unemp" + i;})
        .attr("r", 4)
        .attr("stroke", "#2b7551")
        .attr("stroke-width", 2)
        .attr("fill", "white")
        
        // Three function that change the tooltip when user hover / move / leave a cell
        .on('mouseover', function(d, i) {
            console.log("mouseover on", this);
            
            // de-emphasis lines and circles
            d3.selectAll("path.path_unemp")
              .attr('opacity', de_emphasize_opacity);
            d3.selectAll("circle.pt_unemp20")
              .attr('stroke-opacity', de_emphasize_opacity);
            svg_unemp.selectAll("line.death_bar")
              .attr('opacity', de_emphasize_opacity);
            
            // make the mouseover'd element big
            d3.selectAll("circle.pt_unemp20" + i)
              .transition()
              .duration(75)
              .attr('r', 8)
              .attr('fill', '#333333')
              .attr('stroke-opacity', 0);
          
          // highlight the 2019 point
            d3.selectAll("circle.pt_unemp19" + i)
              .transition()
              .duration(75)
              .attr('r', 6)
              .attr('fill', '#737373')
              .attr('stroke-opacity', 0)
              .attr('opacity', 1);
            
            // this makes the tooltip show
            Tooltip.style("opacity", 1);
          })
          
        .on('mousemove', function(d, i) {
          // this makes the tooltip move
          Tooltip
            .html(
              "<b>" + d.text + "</b><br>" + 
                  Math.round(d.ICSA_2020 * 10, 2) / 10 + " million unemployment claims " 
                  + YoY_diff(d.ICSA_2020, d.ICSA_2019)
                  )
            .style("left", d3.event.pageX + 10 + "px")
            .style("top", d3.event.pageY + "px");
        })
        
        .on('mouseout', function(d, i) {
            console.log("mouseout", this);
            
            
            // return the mouseover'd element to the original format
            d3.selectAll("circle.pt_unemp20" + i)
              .transition()
              .duration(200)
              .attr('r', 4)
              .attr('fill', 'white')
              .attr('stroke-opacity', 1);
              
            // return the 2019 dot
            d3.selectAll("circle.pt_unemp19" + i)
              .transition()
              .duration(200)
              .attr('opacity', 0);
              
            // re-emphasis lines and circles
            d3.selectAll("path.path_unemp")
              .attr('opacity', 1);
            d3.selectAll("path.path_unemp19")
              .attr('opacity', 0.5);
            d3.selectAll("circle.pt_unemp20")
              .attr('stroke-opacity', 1);
            svg_unemp.selectAll("line.death_bar")
              .attr('opacity', 1);
              
              // this makes the tooltip disappear
              Tooltip.style("opacity", 0);
          });
          
  // this removes the circles where we don't have ICSA values
  circle_unemp.filter(function(d) { 
	  console.log(d);
	return d.ICSA_20_exists != "TRUE"}).remove();
	
          
  // flights plot
  
    // Scale the range of the data
    x.domain(d3.extent(data, function(d) { return d.date; }));
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
        .tickSize(my_tickSize)
        .ticks(my_nXticks)
        .tickFormat(d3.timeFormat("%b")));

    // Add Y axis
    svg_flights.append("g")
      .attr("class", "grid")
      .call(d3.axisLeft(y)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nYticks));
      
    // text label for the y axis
    svg_flights.append("text").call(drawYlabel).text("Daily commercial flights (thousands)");

    // add vertical bar for first death
    svg_flights.append("line")
      .call(styleDeath);

    // Add the line
    svg_flights.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#2b7551")
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d.flights) })
        .curve(d3.curveMonotoneX)
        
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
        .attr("class", function(d, i) {return "pt_flights" + i;})
        .attr("r", 4)
        .attr("stroke", "#2b7551")
        .attr("stroke-width", 2)
        .attr("fill", "white")
        
        // Three function that change the tooltip when user hover / move / leave a cell
        .on('mouseover', function(d, i) {
            console.log("mouseover on", this);
            // make the mouseover'd element big
            d3.selectAll("circle.pt_flights" + i)
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
                  Math.round(d.flights * 10, 2) / 10 + " thousand commercial flights")
            .style("left", d3.event.pageX + 10 + "px")
            .style("top", d3.event.pageY + "px");
        })
        
        .on('mouseout', function(d, i) {
            console.log("mouseout", this);
            // return the mouseover'd element to the original format
            d3.selectAll("circle.pt_flights" + i)
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

// wrapper function to update google trends plot
function update(chosen_Y){

//Read the subway and citibike data
d3.csv("/d3/covid-impact/data/sub_citi_unemp_flights.csv",

  // When reading the csv, I must format variables:
  function(d){
    return { date : parseDate(d.date), subway_2020 : d.subway_2020, subway_2019 : d.subway_2019, text : d.text, bike_2020 : d.bike_2020, bike_2019 : d.bike_2019, ICSA : d.ICSA, flights : d.flights, ICSA_exists : d.ICSA_exists, retail_rec : d.retail_rec, groc_pharm : d.groc_pharm, parks : d.parks, transit : d.transit, workplaces : d.workplaces, Residential : d.Residential }
  },

  // Now I can use this dataset:
  function(data) {

	  // google trends plot
  
    // remove existing text
    this.svg_trends.selectAll('grid').remove();
    // remove existing text
    this.svg_trends.selectAll('text').remove();
    // remove existing lines
    this.svg_trends.selectAll('path').remove();
    // remove existing circles
    this.svg_trends.selectAll('circle').remove();
    
  
    // Scale the range of the data
    x.domain(d3.extent(data, function(d) { return d.date; }));
    y.domain( [-0.6, 0.6] );

    // add the X gridlines
    svg_trends.append("g")			
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(make_x_gridlines()
          .tickSize(-height-my_tickSize)
          .tickFormat("")
      );
      
    // add the Y gridlines
    svg_trends.append("g")			
      .attr("class", "grid")
      .call(make_y_gridlines()
          .tickSize(-width-my_tickSize)
          .tickFormat('')
      );
      
    // add the X axis
    svg_trends.append("g")
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(d3.axisBottom(x)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nXticks)
        .tickFormat(d3.timeFormat("%b")));

    // Add Y axis
    svg_trends.append("g")
      .attr("class", "grid")
      .call(d3.axisLeft(y)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nYticks)
        .tickFormat(d3.format(".0%")));
      
    // text label for the y axis
    svg_trends.append("text").call(drawYlabel).text("Daily activity compared to baseline");

    // add vertical bar for first death
    svg_trends.append("line")
      .call(styleDeath);

    // Add the line
    svg_trends.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#2b7551")
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d[chosen_Y]) })
        .curve(d3.curveMonotoneX)
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d[chosen_Y] != "NA"})
        );

    // Add the points
    var circle_trends = svg_trends
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.date) } )
        .attr("cy", function(d) { return y(d[chosen_Y]) } )
        .attr("class", function(d, i) {return "pt_trends" + i;})
        .attr("r", 4)
        .attr("stroke", "#2b7551")
        .attr("stroke-width", 2)
        .attr("fill", "white")
        
        // Three function that change the tooltip when user hover / move / leave a cell
        .on('mouseover', function(d, i) {
            console.log("mouseover on", this);
            // make the mouseover'd element big
            d3.selectAll("circle.pt_trends" + i)
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
                  printableNumber(Math.round(d[chosen_Y] * 100)) + " compared to baseline")
            .style("left", d3.event.pageX + 10 + "px")
            .style("top", d3.event.pageY + "px");
        })
        
        .on('mouseout', function(d, i) {
            console.log("mouseout", this);
            // return the mouseover'd element to the original format
            d3.selectAll("circle.pt_trends" + i)
              .transition()
              .duration(200)
              .attr('r', 4)
              .attr('fill', 'white')
              .attr('stroke-opacity', 1);
              
              // this makes the tooltip disappear
              Tooltip.style("opacity", 0);
          });
          
  // this removes the 'NA' values
  circle_trends.filter(function(d) { 
	  console.log(d);
	return d[chosen_Y] == "NA"; }).remove();
	
	}

);
}




// wrapper function to update google trends plot
function update311(chosen_Y){

 var chosen_Y_2020 = chosen_Y + "_2020";
 var chosen_Y_2019 = chosen_Y + "_2019";

//Read the 311 data
d3.csv("/d3/covid-impact/data/threeOneOne.csv",

  // When reading the csv, I must format variables:
  function(d){
    return { date : parseDate(d.date), text : d.text, Abandoned_vehicle_2020 : d.Abandoned_vehicle_2020, Abandoned_vehicle_2019 : d.Abandoned_vehicle_2019, Blocked_driveway_2020 : d.Blocked_driveway_2020, Blocked_driveway_2019 : d.Blocked_driveway_2019, Consumer_complaint_2020 : d.Consumer_complaint_2020, Consumer_complaint_2019 : d.Consumer_complaint_2019, Derelict_bicycle_2020 : d.Derelict_bicycle_2020, Derelict_bicycle_2019 : d.Derelict_bicycle_2019, Derelict_vehicles_2020 : d.Derelict_vehicles_2020, Derelict_vehicles_2019 : d.Derelict_vehicles_2019, For_hire_vehicle_complaint_2020 : d.For_hire_vehicle_complaint_2020, For_hire_vehicle_complaint_2019 : d.For_hire_vehicle_complaint_2019, General_2020 : d.General_2020, General_2019 : d.General_2019, Illegal_parking_2020 : d.Illegal_parking_2020, Illegal_parking_2019 : d.Illegal_parking_2019, Lost_property_2020 : d.Lost_property_2020, Lost_property_2019 : d.Lost_property_2019, Noise_commercial_2020 : d.Noise_commercial_2020, Noise_commercial_2019 : d.Noise_commercial_2019, Noise_street_sidewalk_2020 : d.Noise_street_sidewalk_2020, Noise_street_sidewalk_2019 : d.Noise_street_sidewalk_2019, Non_emergency_police_matter_2020 : d.Non_emergency_police_matter_2020, Non_emergency_police_matter_2019 : d.Non_emergency_police_matter_2019, Rodent_2020 : d.Rodent_2020, Rodent_2019 : d.Rodent_2019, School_maintenance_2020 : d.School_maintenance_2020, School_maintenance_2019 : d.School_maintenance_2019, Street_condition_2020 : d.Street_condition_2020, Street_condition_2019 : d.Street_condition_2019, Taxi_complaint_2020 : d.Taxi_complaint_2020, Taxi_complaint_2019 : d.Taxi_complaint_2019 }
  },

  // Now I can use this dataset:
  function(data) {

	  // google trends plot
  
    // remove existing text
    this.svg_311.selectAll('grid').remove();
    // remove existing text
    this.svg_311.selectAll('text').remove();
    // remove existing lines
    this.svg_311.selectAll('path').remove();
    // remove existing circles
    this.svg_311.selectAll('circle').remove();
    // remove existing y axis
    this.svg_311.selectAll('g').remove();
    // remove arrow line
    this.svg_311.selectAll('line').remove();
    // remove arrow head
    this.svg_311.selectAll('defs').remove();
    
    // Scale the range of the data
    x.domain(d3.extent(data, function(d) { return d.date; }));
    y.domain( [0, 
      d3.max([
        d3.max(data, function (d) { return d[chosen_Y_2020] * 1.1; }),
        d3.max(data, function (d) { return d[chosen_Y_2019] * 1.1; })
        ])
      ]).nice();

    // add the X gridlines
    svg_311.append("g")			
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(make_x_gridlines()
          .tickSize(-height-my_tickSize)
          .tickFormat("")
      );
      
    // add the Y gridlines
    svg_311.append("g")			
      .attr("class", "grid")
      .call(make_y_gridlines()
          .tickSize(-width-my_tickSize)
          .tickFormat('')
      );
      
    // add the X axis
    svg_311.append("g")
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(d3.axisBottom(x)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nXticks)
        .tickFormat(d3.timeFormat("%b")));

    // Add Y axis
    svg_311.append("g")
      .attr("class", "grid")
      .call(d3.axisLeft(y)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nYticks));
      
    // text label for the y axis
    svg_311.append("text").call(drawYlabel).text("Daily calls");

    // add vertical bar for first death
    svg_311.append("line")
      .call(styleDeath);

    // add label for social distancing calls
    if (chosen_Y == 'Non_emergency_police_matter'){

        function SD_style(selection){
            selection
              .attr("class", "plot_label")
              .attr("text-anchor", "right")  
              .style("font-size", "12px") 
              .style("font-weight", "700") 
              .style("fill", "#333333");
          }
        
        // labels
        svg_311.append("text")
            .call(SD_style)
            .attr("x", (width * 0.74))             
            .attr("y", (height * 0.18))
            .text("Mostly calls to");
        svg_311.append("text")
            .call(SD_style)
            .attr("x", (width * 0.76))             
            .attr("y", (height * 0.24))
            .text("report social");
        svg_311.append("text")
            .call(SD_style)
            .attr("x", (width * 0.67))             
            .attr("y", (height * 0.30))
            .text(" distancing infractions")
            
        // arrow head
        svg_311.append("svg_311:defs").append("svg_311:marker")
            .call(SD_style)
            .attr("id", "triangle")
            .attr("refX", 6)
            .attr("refY", 6)
            .attr("markerWidth", 30)
            .attr("markerHeight", 30)
            .attr("orient", "auto")
            .append("path")
            .attr("d", "M 0 0 12 6 0 12 3 6")
            .style("fill", "#333333");
        
        // line              
        svg_311.append("line")
          .call(SD_style)
          .attr("x1", (width * 0.90))
          .attr("y1", (height * 0.22))
          .attr("x2", (width * 0.94))
          .attr("y2", (height * 0.22))          
          .attr("stroke-width", 1)
          .attr("stroke", "#333333")
          .attr("marker-end", "url(#triangle)");
  
    }

    // Add the 2019 line 
    svg_311.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#333333")
      .attr("stroke-width", 1)
      .attr("opacity", 0.5)
      .attr("class", "path_311 path_31119")
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d[chosen_Y_2019]) })
        .curve(d3.curveMonotoneX)
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d[chosen_Y_2019] != "NA"})
        );

    // Add the line
    svg_311.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#2b7551")
      .attr("class", "path_311")
      .attr("d", d3.line()
        .x(function(d) { return x(d.date) })
        .y(function(d) { return y(d[chosen_Y_2020]) })
        .curve(d3.curveMonotoneX)
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d[chosen_Y_2020] != "NA"})
        );

    // Add the 2019 points
    var circle_trend19 = svg_311
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.date) } )
        .attr("cy", function(d) { return y(d[chosen_Y_2019]) } )
        .attr("class", function(d, i) {return "pt_31119" + i;});
        
    // Add the points
    var circle_trends = svg_311
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.date) } )
        .attr("cy", function(d) { return y(d[chosen_Y_2020]) } )
        .attr("class", function(d, i) {return "pt_31120 pt_31120" + i;})
        .attr("r", 4)
        .attr("stroke", "#2b7551")
        .attr("stroke-width", 2)
        .attr("fill", "white")
        
        // Three function that change the tooltip when user hover / move / leave a cell
        .on('mouseover', function(d, i) {
            console.log("mouseover on", this);
            
            // de-emphasis lines and circles
            d3.selectAll("path.path_311")
              .attr('opacity', de_emphasize_opacity);
            d3.selectAll("circle.pt_31120")
              .attr('stroke-opacity', de_emphasize_opacity);
            svg_311.selectAll("text.plot_label")
              .attr('opacity', de_emphasize_opacity);
            svg_311.selectAll('line.plot_label')
              .attr('opacity', de_emphasize_opacity);
            svg_311.selectAll("line.death_bar")
              .attr('opacity', de_emphasize_opacity);
            
            // make the mouseover'd element big
            d3.selectAll("circle.pt_31120" + i)
              .transition()
              .duration(75)
              .attr('r', 8)
              .attr('fill', '#333333')
              .attr('stroke-opacity', 0);
            
            // highlight the 2019 point
            d3.selectAll("circle.pt_31119" + i)
              .transition()
              .duration(75)
              .attr('r', 6)
              .attr('fill', '#737373')
              .attr('stroke-opacity', 0)
              .attr('opacity', 1);
            
            // this makes the tooltip show
            Tooltip.style("opacity", 1);
          })
          
        .on('mousemove', function(d, i) {
          // this makes the tooltip move
          Tooltip
            .html(
              "<b>" + d.text + "</b><br>" + 
              Math.round(d[chosen_Y_2020] * 10, 2) / 10 + " calls " +
              YoY_diff(d[chosen_Y_2020], d[chosen_Y_2019])
              )
            .style("left", d3.event.pageX + 10 + "px")
            .style("top", d3.event.pageY + "px");
        })
        
        .on('mouseout', function(d, i) {
            console.log("mouseout", this);
            // return the mouseover'd element to the original format
            d3.selectAll("circle.pt_31120" + i)
              .transition()
              .duration(200)
              .attr('r', 4)
              .attr('fill', 'white')
              .attr('stroke-opacity', 1);
              
            // return the 2019 dot
            d3.selectAll("circle.pt_31119" + i)
              .transition()
              .duration(200)
              .attr('opacity', 0);
              
            // re-emphasis lines and circles
            d3.selectAll("path.path_311")
              .attr('opacity', 1);
            d3.selectAll("path.path_31119")
              .attr('opacity', 0.5);              
            d3.selectAll("circle.pt_31120")
              .attr('stroke-opacity', 1);
            svg_311.selectAll("text.plot_label")
              .attr('opacity', 1);
            svg_311.selectAll('line.plot_label')
              .attr('opacity', 1);
            svg_311.selectAll("line.death_bar")
              .attr('opacity', 1);
              
              // this makes the tooltip disappear
              Tooltip.style("opacity", 0);
          });
          
  // this removes the 'NA' values
  circle_trends.filter(function(d) { 
	  console.log(d);
	return d[chosen_Y_2020] == "NA"; }).remove();
	
	// this removes the 'NA' values for the 2019 circles
  circle_trend19.filter(function(d) { 
	  console.log(d);
	return d[chosen_Y_2019]== "NA" }).remove();
	
	}

);
}


update('retail_rec');
update311('Non_emergency_police_matter');

