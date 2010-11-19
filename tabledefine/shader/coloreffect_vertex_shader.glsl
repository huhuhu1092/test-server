attribute vec3 a_position;
attribute vec2 a_tex_coord0;
varying vec2 v_tex_coord0;
uniform mat4 u_wvp_matrix;
void main()
{
    gl_Position = u_wvp_matrix * vec4(a_position, 1.0);
    v_tex_coord0 = a_tex_coord0;
}
