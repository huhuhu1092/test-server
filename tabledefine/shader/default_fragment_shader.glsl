precision mediump float;

varying vec2 v_tex_coord;
uniform sampler2D u_texture;
uniform vec3 u_color;
uniform int u_shading_mode;
void main()
{
    if(u_shading_mode == 0)
    {
        gl_FragColor = vec4(u_color, 1.0);
    }
    else
    {     
        vec2 texcoord = v_tex_coord;
        gl_FragColor = texture2D(u_texture, texcoord);
    } 
}

