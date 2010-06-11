attribute vec3 a_position;
attribute vec2 a_tex_coord;
varying vec2 v_tex_coord;
uniform mat4 u_obj_to_world_matrix;
uniform mat4 u_world_to_view_matrix;
uniform mat4 u_view_to_projective_matrix;
uniform mat4 u_mvp_matrix;
void main()
{
    gl_Position = u_mvp_matrix * vec4(a_position, 1.0);
    //vec4 totalPosition = u_view_to_projective_matrix * u_world_to_view_matrix * u_obj_to_world_matrix * vec4(a_position, 1.0);
    //gl_Position = totalPosition;
    v_tex_coord = a_tex_coord;
}
