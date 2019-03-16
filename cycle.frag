/* {
"osc": 4567,
"audio": true ,
"pixelRatio":1,
} */
precision mediump float; uniform float time; uniform vec2 resolution; uniform float volume;
uniform vec2 mouse;
uniform sampler2D backbuffer; uniform sampler2D samples; uniform sampler2D spectrum;
uniform sampler2D osc_cycle;
#define MAX_DISTANCE 40.
#define MAX_STEPS 100
#define PI 3.14159265359
//float offset=texture2D(spectrum,vec2(pow(abs(uv.x)/3.145/2.,2.),0.)).r;
vec3 hsv2rgb(vec3 c) {vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);}
vec2 c2p(vec2 p){return vec2(atan(p.y,p.x),length(p));}
vec2 p2c(vec2 p){return vec2(cos(p.x),sin(p.x))*p.y;}
vec3 showCycle(float steps, float cycles, float curCycle, float hue, vec2 rad, vec2 coord) {
  float index = mod(curCycle,cycles)/cycles;
  index = floor(index*steps)/steps;
  index *= 2.*PI;
  vec2 phaseMag = c2p(coord);
  phaseMag.x+= PI*1.5;
  phaseMag.x = mod(phaseMag.x,2.*PI);
  if((phaseMag.x<index||index==0.)&&phaseMag.y>rad.x&&phaseMag.y<rad.y) return hsv2rgb(vec3(hue,2./3.,1.));
  return vec3(0.);
}
#define CIRCLES 15.
void main() {
    vec2 uv = gl_FragCoord.xy / resolution;
    uv-=0.5;
    uv.x*=resolution.x/resolution.y;
    float cycle = texture2D(osc_cycle,vec2(0.)).r;
    vec3 color = vec3(0.);
    for(int i=0; i<int(CIRCLES); i++) {
      float index = float(i);
      color += showCycle(64.,pow(2.,index),cycle,index/CIRCLES,vec2(0.+index/CIRCLES,0.06+index/CIRCLES),uv);
    }
    if(length(showCycle(25.*60.,25.*60.,time,0.,vec2(0.01,0.04),uv))>0.) {
      color = vec3(0.);
    }
    gl_FragColor = vec4(color*pow(0.95,c2p(uv).y*40.)/3.,1.);
}
