precision mediump float;
const int FN_MULTIPLY = 0;
const int FN_ADD = 1;
varying vec2 v_tex_coord0;
uniform sampler2D u_texture_background;
uniform sampler2D u_texture_channel;
uniform sampler2D u_texture_texr;
uniform sampler2D u_texture_texg;
uniform sampler2D u_texture_texb;
uniform sampler2D u_texture_texa;

uniform int u_has_texr;
uniform int u_has_texg;
uniform int u_has_texb;
uniform int u_has_texa;

uniform float u_markr_alpha;
uniform float u_markg_alpha;
uniform float u_markb_alpha;
uniform float u_marka_alpha;
uniform float u_background_alpha;
uniform int u_markr_fn;
uniform int u_markg_fn;
uniform int u_markb_fn;
uniform int u_marka_fn;

uniform vec3 u_colora;
uniform vec3 u_colorr;
uniform vec3 u_colorg;
uniform vec3 u_colorb;

vec3 calculateColor(vec3 backgroundColor, float alpha, vec3 foregroundColor, int fn)
{
    if(fn == FN_MULTIPLY)
    {
        float r = alpha * backgroundColor.r * foregroundColor.r;
        float g = alpha * backgroundColor.g * foregroundColor.g;
        float b = alpha * backgroundColor.b * foregroundColor.b;
        r = (1.0 - alpha) * backgroundColor.r + alpha * r;
        g = (1.0 - alpha) * backgroundColor.g + alpha * g;
        b = (1.0 - alpha) * backgroundColor.b + alpha * b;
        return vec3(r, g, b);
    }
    else if(fn == FN_ADD)
    {
        float r = (foregroundColor.r + backgroundColor.r) > 1.0 ? 1.0 : (foregroundColor.r + backgroundColor.r);
        float g = (foregroundColor.g + backgroundColor.g) > 1.0 ? 1.0 : (foregroundColor.g + backgroundColor.g);
        float b = (foregroundColor.b + backgroundColor.b) > 1.0 ? 1.0 : (foregroundColor.b + backgroundColor.b);
        r = (1.0 - alpha) * backgroundColor.r + alpha * r;
        g = (1.0 - alpha) * backgroundColor.g + alpha * g;
        b = (1.0 - alpha) * backgroundColor.b + alpha * b;
        return vec3(r, g, b);
    }
    return vec3(0, 0, 0);
}
void main()
{
    vec4 backgroundColor = texture2D(u_texture_background, v_tex_coord0);
    vec4 channelColor = texture2D(u_texture_channel, v_tex_coord0);
    vec3 foregroundColor = u_colorr;
    if(u_has_texr == 1)
    {
        vec4 texrColor = texture2D(u_texture_texr, v_tex_coord0);
        foregroundColor = foregroundColor * texrColor.rgb;
    }
    vec3 color = calculateColor(backgroundColor.rgb, channelColor.r * u_markr_alpha, foregroundColor, u_markr_fn);
    foregroundColor = u_colorg;
    if(u_has_texg == 1)
    {
        vec4 texgColor = texture2D(u_texture_texg, v_tex_coord0);
        foregroundColor = foregroundColor * texgColor.rgb;
    }
    color = calculateColor(color, channelColor.g  * u_markg_alpha, foregroundColor, u_markg_fn);
    foregroundColor = u_colorb;
    if(u_has_texb == 1)
    {
        vec4 texbColor = texture2D(u_texture_texb, v_tex_coord0);
        foregroundColor = foregroundColor * texbColor.rgb;
    }
    color = calculateColor(color, channelColor.b * u_markb_alpha, foregroundColor, u_markb_fn);
    foregroundColor = u_colora;
    if(u_has_texa == 1)
    {
        vec4 texaColor = texture2D(u_texture_texa, v_tex_coord0);
        foregroundColor = foregroundColor * texaColor.rgb;
    }
    color = calculateColor(color, channelColor.a * u_marka_alpha, foregroundColor, u_marka_fn);
    gl_FragColor = vec4(color, backgroundColor.a * u_background_alpha);
}
