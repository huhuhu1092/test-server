attribute vec3 a_position;
attribute vec2 a_tex_coord;
uniform mat4 u_wvp_matrix;
varying vec2 v_tex_coord;
void main()
{
    gl_Position = u_wvp_matrix * vec4(a_position, 1.0);
    v_tex_coord = a_tex_coord;
}

