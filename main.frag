/* {
"osc": 4567,
"audio": true ,
"pixelRatio":3,
} */
precision mediump float; uniform float time; uniform vec2 resolution; uniform float volume;
uniform vec2 mouse;
uniform sampler2D backbuffer; uniform sampler2D samples; uniform sampler2D spectrum;
#define MAX_DISTANCE 40.
#define MAX_STEPS 100
//float offset=texture2D(spectrum,vec2(pow(abs(uv.x)/3.145/2.,2.),0.)).r;
vec3 hsv2rgb(vec3 c) {vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);}

vec2 c2p(vec2 p){return vec2(atan(p.y,p.x),length(p));}
vec2 p2c(vec2 p){return vec2(cos(p.x),sin(p.x))*p.y;}
vec3 c2s(vec3 p){float radius=sqrt(p.x*p.x+p.y*p.y+p.z*p.z);return vec3(atan(p.y, p.x),acos(p.z / radius),radius);}
vec3 s2c(vec3 p){return vec3(cos(p.x)*cos(p.y)*p.z,sin(p.x)*cos(p.y)*p.z,sin(p.y)*p.z);}
vec2 rot(vec2 v,float a){float s=sin(a);float c=cos(a);return mat2(c,-s,s,c)*v;}
vec3 look(vec2 xy, vec3 origin, vec3 target){vec3 up=normalize(vec3(0.,1.,0.));vec3 fwd=normalize(target-origin);vec3 right=normalize(cross(fwd,up));up=normalize(cross(fwd,right));return normalize(fwd+right*xy.x+up*xy.y);}
float map(vec3 pos)
{
  vec3 old = pos;

  for(int i=0;i<7;i++) {
    pos=abs(pos)-0.08;
    pos.zy=p2c(c2p(pos.zy)+vec2(2.1+1.-texture2D(spectrum,vec2(.03)).r*1.5,0.));
    pos.xz = c2p(pos.xz);
    pos.x = 0.1;
    pos.xz = p2c(pos.xz);
    pos-=vec3(0.08,0.,0.);
    pos.xyz=pos.zxy;
    if(i>int(atan(texture2D(spectrum,vec2(0.05))*1.8)*7.)) break;
    pos*=1.1;
  }
  //return length(abs(pos)-0.1)-atan(texture2D(spectrum,vec2(0.01)).r)*0.1-0.1;
  return length(pos)-0.03*texture2D(spectrum,vec2(.01)).r;
}
vec3 normal(vec3 p,float epsilon){vec2 e=vec2(epsilon,0.);return normalize(vec3(map(p+e.xyy)-map(p-e.xyy),map(p+e.yxy)-map(p-e.yxy),map(p+e.yyx)-map(p-e.yyx)));}
float march(vec3 origin,vec3 ray,int steps)
{
    float t=.05;
    for(int i=0;i<MAX_STEPS; i++)
    {
		    float d=map(origin+ray*t);
        if(d<0.002||d>=MAX_DISTANCE||i>=steps||i>int(atan(volume*texture2D(spectrum,vec2(0.2))*2.)*100.+10.)) break;
        t+=d*0.95;
    }
    return min(t,MAX_DISTANCE);
}
void main() {
    vec2 uv = gl_FragCoord.xy / resolution;
    uv-=0.5;
    uv.x*=resolution.x/resolution.y;
    vec3 camera=vec3(min(1.3,max(-.1,2.-atan(volume*1.1))));
    //camera=vec3(sin(time/4.),sin(time/4.),cos(time/4.))*(1.+texture2D(spectrum,vec2(.01)).r*1.5);
    vec3 ray=look(uv,camera,vec3(0.));
    float dist=march(camera,ray,MAX_STEPS);
    vec3 hit=camera+ray*dist;
    float ao=pow(1.-dist/MAX_DISTANCE,20.);
    float diffuse=clamp(dot(normal(hit,0.01*dist),normalize(camera)),0.5,1.);
    float shade=diffuse*ao*0.5+ao*0.5;
    shade = pow(shade,5.*min(1.,atan(volume*5.+0.65)))*20.;
    vec3 color=hsv2rgb(vec3(length(hit)*0.1+0.6,sin(length(hit*ao)*5.)*0.2+.8,shade*2.));
    float coef=pow(atan(volume)*texture2D(spectrum,vec2(.015)).r,.5)+0.1;
    color = color*coef + texture2D(backbuffer,gl_FragCoord.xy/resolution).xyz*(1.-coef);
    gl_FragColor = vec4(color,1.);
}
