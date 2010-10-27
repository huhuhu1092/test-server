precision mediump float;

const int COLOR_MODE = 0;
const int TEXTURE0_MODE = 1;
const int TEXTURE1_MODE = 2;
const int TEXTURE2_MODE = 3;
const int TEXTURE3_MODE = 4;
const int COLOR_TEXTURE0_MODE = 5;
const int COLOR_TEXTURE1_MODE = 6;
const int COLOR_TEXTURE2_MODE = 7;
const int COLOR_TEXTURE3_MODE = 8;
const int TEXTURE0_TEXTURE1_MODE = 9;
const int TEXTURE0_TEXTURE2_MODE = 10;
const int TEXTURE0_TEXTURE3_MODE = 11;
const int TEXTURE1_TEXTURE2_MODE = 12;
const int TEXTURE1_TEXTURE3_MODE = 13;
const int TEXTURE2_TEXTURE3_MODE = 14;
const int COLOR_TEXTURE0_TEXTURE1_MODE = 15;
const int COLOR_TEXTURE0_TEXTURE2_MODE = 16;
const int COLOR_TEXTURE0_TEXTURE3_MODE = 17;
const int COLOR_TEXTURE1_TEXTURE2_MODE = 18;
const int COLOR_TEXTURE1_TEXTURE3_MODE = 19;
const int COLOR_TEXTURE2_TEXTURE3_MODE = 20;
const int TEXTURE0_TEXTURE1_TEXTURE2_MODE = 21;
const int TEXTURE0_TEXTURE1_TEXTURE3_MODE = 22;
const int TEXTURE0_TEXTURE2_TEXTURE3_MODE = 23;
const int TEXTURE1_TEXTURE2_TEXTURE3_MODE = 24;
const int COLOR_TEXTURE0_TEXTURE1_TEXTURE2_MODE = 25;
const int COLOR_TEXTURE0_TEXTURE1_TEXTURE3_MODE = 26;
const int COLOR_TEXTURE1_TEXTURE2_TEXTURE3_MODE = 27;
const int COLOR_TEXTURE0_TEXTURE2_TEXTURE3_MODE = 28;
const int TEXTURE0_TEXTURE1_TEXTURE2_TEXTURE3_MODE = 29;
const int COLOR_TEXTURE0_TEXTURE1_TEXTURE2_TEXTURE3_MODE = 30;

varying vec2 v_tex_coord0;
varying vec2 v_tex_coord1;
varying vec2 v_tex_coord2;
varying vec2 v_tex_coord3;

uniform sampler2D u_texture0;
uniform sampler2D u_texture1;
uniform sampler2D u_texture2;
uniform sampler2D u_texture3;
uniform int u_tex1_coord_same_as_tex0;
uniform int u_tex2_coord_same_as_tex0;
uniform int u_tex3_coord_same_as_tex0;
uniform int u_tex_combine_mode;
uniform vec3 u_color;
void main()
{
    if(u_tex_combine_mode == COLOR_MODE)
    {
        gl_FragColor = vec4(u_color, 1.0);
    }
    else if(u_tex_combine_mode == TEXTURE0_MODE)
    {     
        vec2 texcoord = v_tex_coord0;
/*
            vec2 v1 = v_tex_coord1;
            vec2 v2 = v_tex_coord2;
            vec2 v3 = v_tex_coord3;
            int u1 = u_tex2_coord_same_as_tex0;
            int u2 = u_tex3_coord_same_as_tex0;
            if(u1 == u2)
                v1 = vec2(1, 1);
            vec4 cc1 = texture2D(u_texture1, texcoord);
            cc1 = texture2D(u_texture2, texcoord);
            cc1 = texture2D(u_texture3, texcoord);
*/
        gl_FragColor = texture2D(u_texture0, texcoord);
    } 
    else  if(u_tex_combine_mode == TEXTURE0_TEXTURE1_MODE)
    {
        vec2 texcoord = v_tex_coord0;
        vec4 basecolor = texture2D(u_texture0, texcoord);
        vec4 texcolor1;
        if(u_tex1_coord_same_as_tex0 == 1)
        {
            texcolor1 = texture2D(u_texture1, v_tex_coord1);
        }
        else
        {
            texcolor1 = texture2D(u_texture1, v_tex_coord0);
        }
        gl_FragColor = basecolor * texcolor1;
    }
    else if(u_tex_combine_mode == COLOR_TEXTURE1_TEXTURE2_MODE)
    {
        vec4 color1;
        vec4 color2;
        if(u_tex1_coord_same_as_tex0 == 1)
        {
            color1 = texture2D(u_texture1, v_tex_coord1);
        }
        else
        {
            color1 = texture2D(u_texture1, v_tex_coord0); 
        }
        if(u_tex2_coord_same_as_tex0 == 1)
        {
            color2 = texture2D(u_texture2, v_tex_coord2);
        }
        else
        {
            color2 = texture2D(u_texture2, v_tex_coord0); 
        }
        gl_FragColor = vec4(u_color.r * color1.r * color2.r , u_color.g * color1.g * color2.g, u_color.b * color1.b * color2.b, color1.a);
    }
    else if(u_tex_combine_mode == TEXTURE2_TEXTURE3_MODE)
    {
        vec4 color1;
        vec4 color2;
        if(u_tex2_coord_same_as_tex0 == 1)
        {
            color1 = texture2D(u_texture2, v_tex_coord2);
        }
        else
        {
            color1 = texture2D(u_texture2, v_tex_coord0); 
        }
        if(u_tex3_coord_same_as_tex0 == 1)
        {
            color2 = texture2D(u_texture3, v_tex_coord3);
        }
        else
        {
            color2 = texture2D(u_texture3, v_tex_coord0); 
        }       
        gl_FragColor = vec4(color1.r * color2.r, color1.g * color2.g, color1.b * color2.b, 1.0);
    }
}
