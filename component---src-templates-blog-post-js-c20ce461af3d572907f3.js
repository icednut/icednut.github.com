(window.webpackJsonp=window.webpackJsonp||[]).push([[8],{157:function(e,t,a){"use strict";a.r(t),a.d(t,"pageQuery",function(){return h});var n=a(7),r=a.n(n),i=a(0),l=a.n(i),o=a(165),s=a(186),c=a.n(s),m=a(174),u=a(168),d=a(169),p=a(166),g=a(175),f=function(e){function t(){return e.apply(this,arguments)||this}return r()(t,e),t.prototype.render=function(){var e=this.props.data.markdownRemark,t=this.props.location.pathname,a=this.props.data.site.siteMetadata.title,n=this.props.data.site.siteMetadata.siteUrl,r=this.props.pageContext,i=r.previous,s=r.next,f=e.frontmatter;return l.a.createElement(l.a.Fragment,null,l.a.createElement(u.a,{location:this.props.location,title:a},l.a.createElement(d.a,{title:f.title,description:f.description||e.excerpt}),l.a.createElement("div",{className:"d-flex flex-column justify-content-end mt-5 mb-5 pt-5 pb-5",style:{textAlign:"right"}},l.a.createElement("div",{className:"d-flex justify-content-end"},l.a.createElement("h1",{style:{maxWidth:Object(p.a)(35),color:"#17a2b8"}},f.title)),l.a.createElement("div",{className:"d-flex justify-content-end align-items-center"},l.a.createElement("div",{style:{fontSize:"13px",color:"#999999"}},f.date),l.a.createElement("div",{className:"d-flex"},f.tags&&f.tags.length>0&&f.tags.map(function(e){return l.a.createElement(o.a,{to:"/tags/"+Object(g.kebabCase)(Object(g.toLower)(e)),className:"badge badge-info pt-1 pb-1 pr-2 pl-2 ml-1",key:e},l.a.createElement(m.a,{icon:"tag",size:"xs",className:"mr-1"}),e)})))),l.a.createElement("div",{dangerouslySetInnerHTML:{__html:e.html}}),l.a.createElement("div",{className:"d-flex justify-content-between pt-5 pb-3 mt-5 mb-3"},l.a.createElement("div",{style:{maxWidth:"50%"}},i&&l.a.createElement(l.a.Fragment,null,l.a.createElement("div",{style:{fontWeight:"bold"}},"Next"),l.a.createElement(o.a,{style:{boxShadow:"none",textDecoration:"none",color:"#17a2b8"},to:i.fields.slug,rel:"prev"},i.frontmatter.title))),l.a.createElement("div",{style:{maxWidth:"50%"}},s&&l.a.createElement("div",{style:{textAlign:"right"}},l.a.createElement("div",{style:{fontWeight:"bold"}},"Prev"),l.a.createElement(o.a,{style:{boxShadow:"none",textDecoration:"none",color:"#17a2b8"},to:s.fields.slug,rel:"next"},s.frontmatter.title)))),l.a.createElement(c.a,{identifier:e.id,title:e.frontmatter.title,url:""+n+t})))},t}(l.a.Component);t.default=f;var h="2160764432"},164:function(e,t,a){var n;e.exports=(n=a(167))&&n.default||n},165:function(e,t,a){"use strict";var n=a(0),r=a.n(n),i=a(4),l=a.n(i),o=a(34),s=a.n(o);a.d(t,"a",function(){return s.a});a(164),r.a.createContext({});l.a.object,l.a.string.isRequired,l.a.func,l.a.func},166:function(e,t,a){"use strict";a.d(t,"a",function(){return s});var n=a(171),r=a.n(n),i=a(172),l=a.n(i),o=new r.a(l.a);var s=o.rhythm;o.scale},167:function(e,t,a){"use strict";a.r(t);a(35);var n=a(0),r=a.n(n),i=a(4),l=a.n(i),o=a(56),s=a(2),c=function(e){var t=e.location,a=s.default.getResourcesForPathnameSync(t.pathname);return a?r.a.createElement(o.a,Object.assign({location:t,pageResources:a},a.json)):null};c.propTypes={location:l.a.shape({pathname:l.a.string.isRequired}).isRequired},t.default=c},168:function(e,t,a){"use strict";var n=a(7),r=a.n(n),i=a(166),l=a(0),o=a.n(l),s=function(){return o.a.createElement("footer",{className:"mt-3"},"© ",(new Date).getFullYear()," Wan Geun Lee, all rights reserved.")},c=a(165),m=(a(153),function(e){var t=e.title,a=e.currentMenu;return o.a.createElement("div",{className:"d-flex justify-content-between mb-3"},o.a.createElement("div",{className:"d-flex align-items-center"},o.a.createElement(c.a,{style:{boxShadow:"none",textDecoration:"none",color:"inherit",fontWeight:"bold"},to:"/"},t),o.a.createElement("div",{className:"mr-3 ml-3",style:{color:"#999999"}}," | "),o.a.createElement(c.a,{className:"tags"===a?"blog-menu blog-menu__highlight":"blog-menu",to:"/tags"},"tags")),o.a.createElement("div",{className:"align-self-end"},o.a.createElement(c.a,{className:"devlog"===a?"blog-menu blog-menu__highlight ml-4":"blog-menu ml-4",to:"/category/devlog"},"Dev-log"),o.a.createElement(c.a,{className:"studynote"===a?"blog-menu blog-menu__highlight ml-4":"blog-menu ml-4",to:"/category/studynote"},"Study Note")))}),u=function(e){function t(){return e.apply(this,arguments)||this}return r()(t,e),t.prototype.render=function(){var e=this.props,t=e.title,a=e.children,n=e.currentMenu;return o.a.createElement("div",{style:{marginLeft:"auto",marginRight:"auto",maxWidth:Object(i.a)(47),padding:Object(i.a)(1.5)+" "+Object(i.a)(.75)}},o.a.createElement(m,{title:t,currentMenu:n}),o.a.createElement("hr",{style:{margin:0}}),o.a.createElement("main",null,o.a.createElement("div",{className:"mb-5 mt-5"},a)),o.a.createElement("hr",{style:{margin:0}}),o.a.createElement(s,null))},t}(o.a.Component);t.a=u},169:function(e,t,a){"use strict";var n=a(170),r=a(0),i=a.n(r),l=a(4),o=a.n(l),s=a(173),c=a.n(s);function m(e){var t=e.description,a=e.lang,r=e.meta,l=e.title,o=n.data.site,s=t||o.siteMetadata.description;return i.a.createElement(c.a,{htmlAttributes:{lang:a},title:l,titleTemplate:"%s | "+o.siteMetadata.title,meta:[{name:"description",content:s},{property:"og:title",content:l},{property:"og:description",content:s},{property:"og:type",content:"website"},{name:"twitter:card",content:"summary"},{name:"twitter:creator",content:o.siteMetadata.author},{name:"twitter:title",content:l},{name:"twitter:description",content:s}].concat(r)})}m.defaultProps={lang:"en",meta:[],description:""},m.propTypes={description:o.a.string,lang:o.a.string,meta:o.a.arrayOf(o.a.object),title:o.a.string.isRequired},t.a=m},170:function(e){e.exports={data:{site:{siteMetadata:{title:"Icednut's Notes",description:"A starter blog demonstrating what Gatsby can do.",author:"Will Lee"}}}}},186:function(e,t,a){"use strict";var n=a(8);t.__esModule=!0,t.default=void 0;var r=n(a(76)),i=n(a(7)),l=n(a(0)),o=n(a(4));a(158);var s=function(e){function t(t){var a;return(a=e.call(this,t)||this).state=t,a.shortname="icednut",a}(0,i.default)(t,e);var a=t.prototype;return a.componentWillReceiveProps=function(e){this.setState(e)},a.componentWillMount=function(){if("undefined"!=typeof window&&window.document){var e=this;window.disqus_config=function(){this.page.identifier=e.state.identifier,this.page.title=e.state.title,this.page.url=e.state.url};var t=document.createElement("script");t.src="//"+this.shortname+".disqus.com/embed.js",t.async=!0,document.body.appendChild(t)}},a.render=function(){var e=this.props;return l.default.createElement("div",(0,r.default)({id:"disqus_thread"},e,{__source:{fileName:"/Users/brettstevenson/Desktop/Folder/gatsby-plugin-workspace/gatsby-plugin-disqus/src/index.js",lineNumber:36},__self:this}))},t}(l.default.Component);t.default=s,s.propTypes={identifier:o.default.string,title:o.default.string,url:o.default.string}}}]);
//# sourceMappingURL=component---src-templates-blog-post-js-c20ce461af3d572907f3.js.map