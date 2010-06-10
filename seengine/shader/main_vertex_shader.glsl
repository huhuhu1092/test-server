attribute vec4 a_position;
attribute vec2 a_tex_coord;
varing vec2 v_tex_coord;
uniform mat4 u_obj_to_world_matrix;
uniform mat4 u_world_to_view_matrix;
uniform mat4 u_view_to_projective_matrix;

void main()
{
    gl_Position = v_view_to_projective_matrix * u_world_to_view_matrix * u_obj_to_world_matrix * a_position;
    v_tex_coordinate = a_tex_coord;
}
