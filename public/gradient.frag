#ifdef GL_ES
precision mediump float;
#endif

uniform vec2 u_resolution;
uniform float max_distance;
uniform float min_distance;

void main() {

    vec2 centre = vec2(u_resolution.x / 2.0, u_resolution.y / 2.0);

    gl_FragColor = vec4(1.0, 0.0, 10.0/255.0, clamp(distance(centre, gl_FragCoord.xy) - min_distance, 0.0, max_distance) / max_distance);
}