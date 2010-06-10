precision mediump float;
varing vec2 v_tex_coord;
uniform sampler2D u_texture;
uniform int u_shading_mode;/**1: texture, else: color*/
uniform vec4f u_color;
void main()
{
    if(u_using_texture == 1)
    { 
        gl_FragColor = texture2D(u_texture, v_tex_coord);
    }
    else
    { 
        gl_FragColor = u_color;
    }
}
