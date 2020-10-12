// node -i -e "$(< geopoly.js)"

/* references:
https://www.nosco.ch/mathematics/en/earth-coordinates.php
https://en.wikipedia.org/wiki/Haversine_formula
https://keisan.casio.com/exec/system/1224587128
*/

// This file uses the coordinate format [lng,lat] in degrees.

var geopoly = function(){}

const r = 6371000;  // radius of Earth (longer, 6378137, at equator)
const dtr = Math.PI/180;  // degrees to radians

metric = function(pos) {
    /*
      sage: var('r phi lam')
      (r, phi, lam)
      sage: x=r*cos(phi)*cos(lam)
      sage: y=r*cos(phi)*sin(lam)
      sage: z=r*sin(phi)
      sage: M=matrix([[diff(i,j) for i in (x,y,z)] for j in (phi,lam)])
      sage: (M*M.transpose()).simplify_trig()
      [           r^2              0]
      [             0 r^2*cos(phi)^2]

      One must scale by (pi/180)^2 to convert between degrees and radians.
    */
    
    let [lng,lat] = pos;
    return [[Math.pow(r * Math.cos(lat*dtr) * dtr, 2), 0],
	    [0, Math.pow(r * dtr, 2)]];
}

geopoly.distance = function(position1, position2) {
    let [lng1,lat1] = position1;
    let [lng2,lat2] = position2;
    
    let M = metric(position1);
    let [lng,lat] = [lng1-lng2,lat1-lat2]
    return Math.sqrt(M[0][0]*lng*lng + M[0][1]*lng*lat + M[1][0]*lat*lng + M[1][1]*lat*lat);
}

/*geopoly.exact_distance = function(position1, position2) {
    let [lng1,lat1] = position1;
    let [lng2,lat2] = position2;
    
    // Haversine formula
    let distance_in_m = 2*r*Math.asin(Math.sqrt(
        Math.pow(Math.sin((lat1-lat2)/2*dtr), 2) +
	    Math.cos(lat1*dtr)*Math.cos(lat2*dtr)*Math.pow(Math.sin((lng1-lng2)/2*dtr), 2)
    ))
    return distance_in_m;
}*/

geopoly.perimeter = function(arrayOfPolygonPositions, metersDistance) {
    if (metersDistance > 0) {
	// grow
    } else {
	// shrink
    }
    return polygon;
}
    
geopoly.area = function(arrayOfPolygonPositions) {
    /*
      sage: var('r phi lam')
      (r, phi, lam)
      sage: x=r*cos(phi)*cos(lam)
      sage: y=r*cos(phi)*sin(lam)
      sage: z=r*sin(phi)
      sage: dphi,dlam=[[diff(i,j) for i in (x,y,z)] for j in (phi,lam)]
      sage: A=vector(dphi).cross_product(vector(dlam))
      sage: A.norm().simplify_trig()
      sqrt(r^4*cos(phi)^2)
      
      So one must scale by r^2*cos(phi), where phi is the latitude in radians.
      One must scale by (pi/180)^2 to convert between degrees and radians.
      Assuming the polygon is much smaller than Earth, one can then work with
      the [lng,lat] coordinates as if they are planar.
    */
    const poly = arrayOfPolygonPositions;
    const n = poly.length;
    const center = poly.reduce(((sum,x) => [sum[0]+x[0]/n,sum[1]+x[1]/n]), [0,0]);
    
    let a = 0;
    for (let i = 0; i < n; i++) {
	let j = (i+1)%n;
	a += (poly[i][0]*poly[j][1] - poly[i][1]*poly[j][0])/2;
    }
    
    const [center_lng, center_lat] = center;
    const squareMeters = a * Math.pow(r * Math.PI/180, 2) * Math.cos(center_lat*dtr);
    return squareMeters;
},

geopoly.closeto = function(definedPosition, arrayOfPossiblePositions, meters) {
    return arrayOfIndexesFromArray;
}

