/**
 * Created by sjp23 on 13/09/2018.
 */


function make_download() {
    //pull all the form fields into a single json object
    var title = $('#title').val();
    var description = $('#description').val();
    var authors = $('#authors input');
    console.log(authors);
    authors_list  = [];
    // start at author 2 beacuse of the hidden template
    for (var i=2; i < authors.length; i += 2) {
        authors_list.push({"firstname": authors[i].value, "surname": authors[i+1].value});
        console.log(i, authors[i].value);
    }
    var bbox = {"north": $('#north').val(), "south": $('#south').val(),
                "east": $('#east').val(), "west": $('#west').val(),};
    var time_range = {"start": $('#start').val(), "end": $('#end').val()};
     var lineage = $('#lineage').val();
     var quality = $('#quality').val();
    var docs = $('#docs input');
    var docs_list  = [];
    // start at doc 2 beacuse of the hidden template
    for (var i=2; i < docs.length; i += 2) {
        docs_list.push({"title": docs[i].value, "url": docs[i+1].value});
    }
     var project_cat_url = $('#project_cat_url').val();
    var project_title = $('#project_title').val();
    var project_description = $('#project_description').val();
    var project_pi_firstname = $('#project_pi_firstname').val();
    var project_pi_surname = $('#project_pi_surname').val();
    var funder = $('#funder').val();
    var grant_number = $('#grant_number').val();
    var project = {"catalogue_url": project_cat_url, "title": project_title,
        "description": project_description,
        "PI": {"firstname": project_pi_firstname, "surname": project_pi_surname},
        "funder": funder, "grant_number": grant_number};

    var instrument_cat_url = $('#instrument_cat_url').val();
    var instrument_title = $('#instrument_title').val();
    var instrument_description = $('#instrument_description').val();
    var instrument = {"catalogue_url": instrument_cat_url, "title": instrument_title,
        "description": instrument_description};

    var computation_cat_url = $('#computation_cat_url').val();
    var computation_title = $('#computation_title').val();
    var computation_description = $('#computation_description').val();
    var computation = {"catalogue_url": computation_cat_url, "title": computation_title,
        "description": computation_description};


   return {"title": title, "description": description, "authors": authors_list,
    "bbox": bbox, "time_range": time_range, "lineage": lineage, "quality": quality,
    "docs": docs_list, "project": project, "instrument": instrument, "computation": computation};

}



function download() {
    var filename = "metadata.yaml";
    var obj = make_download();
    var obj_str = JSON.stringify(obj, null, 2);
    console.log(obj);
  var element = document.createElement('a');
  element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(obj_str));
  element.setAttribute('download', filename);

  element.style.display = 'none';
  document.body.appendChild(element);

  element.click();

  document.body.removeChild(element);
}

// Start file download.
//download("hello.txt","This is the content of my file :)");


$(".tog").click(function(event){
   var help_div = $('#' + event.currentTarget.id + " div");
    console.log(help_div);
   help_div.toggle( 'slow', function(){ });

});

$(".tog").click();

// list form
function add_row(div_name) {
    control_div = $('#'+ div_name);
    template_row = $('#'+ div_name + ' .template');
    console.log(control_div);
    console.log(template_row.html());

    template_row.clone().removeClass("template").toggle().appendTo(control_div);
   // control_div.append(template_row.html())

}

// add three blank rows for the authors
add_row("authors");
add_row("authors");
add_row("authors");

