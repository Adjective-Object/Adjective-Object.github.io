			//TODO shift this to trigger on div switch

			var currentfocus = "#home";

			var focusWindow = function(){
				console.log("focusing window on "+currentfocus);
				var elem = $(currentfocus);

				$("#containerview").css('transform',
						"translate(" + (-elem.position().left/elem.parent().width()*100) + '%' + ", "
									 + (-elem.position().top/elem.parent().height()*100)+ "%)");
				console.log('transform',
						"translate(" + (-elem.position().left/elem.parent().width()*100) + '%' + ", "
									 + (-elem.position().top/elem.parent().height()*100)+ "%)");
			};

			var jumpWindow = function(){
				/* TODO need to fix dis */
				$("#containerview").removeClass("animated");
				focusWindow();
				setTimeout(function(){
					$("#containerview").addClass("animated");
				}, 0);
			}

			$(window).load(function(){
				$(".sheathed").each(function(index, obj){
					//console.log(obj)
					//console.log($(obj).index())
					window.setTimeout(function(){
						$(obj).removeClass("sheathed");
						$(obj).addClass("revealed");
					},$(obj).index()*75);
				});
			})

			$(document).ready(function(){


				var s = location.href.split("#");
				console.log(s[s.length-1]);
				console.log(s[s.length-1].indexOf("!")!=0);
				if(s.length>1 && s[s.length-1].indexOf("!")!=0 && ( "#"+s[s.length-1] ).length>0) {
					currentfocus = "#"+s[s.length-1];
					jumpWindow();
				}

				window.onhashchange = function(){
					var s = location.href.split("#");
					if(s.length>1 && s[s.length-1].indexOf("!")!=0 && ( "#"+s[s.length-1] ).length>0) {
						currentfocus = "#"+s[s.length-1];
					}else{
						currentfocus = "#home"
					}
					focusWindow();
				}

				$(window).resize(function(evt){
					focusWindow();
				});

			});
