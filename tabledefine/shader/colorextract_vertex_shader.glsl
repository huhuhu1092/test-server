attribute vec3 a_position;
attribute vec2 a_tex_coord0;
attribute vec2 a_tex_coord1;
attribute vec2 a_tex_coord2;
attribute vec2 a_tex_coord3;
varying vec2 v_tex_coord0;
varying vec2 v_tex_coord1;
varying vec2 v_tex_coord2;
varying vec2 v_tex_coord3;

//uniform mat4 u_obj_to_world_matrix;
//uniform mat4 u_world_to_view_matrix;
//uniform mat4 u_view_to_projective_matrix;
uniform mat4 u_wvp_matrix;
void main()
{
    gl_Position = u_wvp_matrix * vec4(a_position, 1.0);
    //vec4 totalPosition = u_view_to_projective_matrix * u_world_to_view_matrix * u_obj_to_world_matrix * vec4(a_position, 1.0);
    //gl_Position = totalPosition;
    v_tex_coord0 = a_tex_coord0.st;
    v_tex_coord1 = a_tex_coord1.st;
    v_tex_coord2 = a_tex_coord2.st;
    v_tex_coord3 = a_tex_coord3.st;
}