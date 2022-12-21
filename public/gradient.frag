#ifdef GL_ES
precision mediump float;
#endif

uniform float u_display_density;
uniform vec2 u_resolution;
uniform float max_distance;
uniform float min_distance;

void main() {

    vec2 scaled_resolution = vec2(u_resolution.x * u_display_density, u_resolution.y * u_display_density);
    float scaled_max_distance = max_distance * u_display_density;
    float scaled_min_distance = min_distance * u_display_density;

    vec2 centre = vec2(scaled_resolution.x / 2.0, scaled_resolution.y / 2.0);

    gl_FragColor = vec4(1.0, 0.0, 10.0/255.0, clamp(distance(centre, gl_FragCoord.xy) - scaled_min_distance, 0.0, scaled_max_distance) / scaled_max_distance);
}