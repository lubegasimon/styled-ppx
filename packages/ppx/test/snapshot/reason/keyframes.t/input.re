let animation = [%keyframe {|
  from  { opacity: 0 }
  100% { opacity: 1}
|}];

module FadeIn = [%styled.section {|
  animation-name: $(animation);
|}];
