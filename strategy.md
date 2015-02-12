---
layout: page
title: Strategy
---

Following are automated Strategy back-test and signal reports,
based on strategies discussed in blogs and at SSRN, that are automatically updated each night:

<ul>
{% for post in site.pages %}
	{% if post.url contains 'Strategy-' %}
	
	 {% assign report_name = post.url | split:'/' | last | remove:'.html' %}	
<li><a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a>
<a href="{{ BASE_PATH }}{{ '/public/images/' | append:report_name | append:'/' | append:report_name | append:'.pdf' }}">[PDF]</a></li>
	{% endif %}
{% endfor %}
</ul>
