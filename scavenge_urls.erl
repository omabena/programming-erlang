-module(scavenge_urls).

-export([bin2urls/1, urls2htmlFile/2, urls2html/1]).

urls2htmlFile(Urls, File) ->
    file:write_file(File, urls2html(Urls)).

bin2urls(Bin) -> gather_urls(binary_to_list(Bin), []).

urls2html(Urls) -> [h1("Urls"), make_list(Urls)].

h1(Title) -> ["<h1>", Title, "</h1>\n"].

make_list(L) ->
    ["<ul>\n",
     lists:map(fun (I) -> ["<li>", I, "</li>\n"] end, L),
     "</ul>\n"].

gather_urls("<a href" ++ T, L) ->
    {Url, T1} = collect_url_body(T,
                                 lists:reverse("<a href")),
    gather_urls(T1, [Url | L]);
gather_urls([_ | T], L) -> gather_urls(T, L);
gather_urls([], L) -> L.

collect_url_body("</a>" ++ T, L) ->
    {lists:reverse(L, "</a>"), T};
collect_url_body([H | T], L) ->
    collect_url_body(T, [H | L]);
collect_url_body([], _) -> {[], []}.
