---
layout: page
title: Tools
---

Following are automated researcg reports that are created each night:

<ul>
{% for post in site.pages %}
	{% if post.url contains 'Tool-' %}
<li><a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
	{% endif %}
{% endfor %}
</ul>
