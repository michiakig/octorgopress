{% codeblock %}
val getc: string -> (char, int) StringCvt.reader =
   fn s => fn i =>
      if (i < String.size s)
         then SOME(String.sub(s, i), i+1)
      else NONE
{% endcodeblock %}
