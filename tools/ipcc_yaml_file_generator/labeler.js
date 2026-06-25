/**
 * Created by sjp23 on 13/09/2018.
 * Updated by jesse on 24/06/2026
 */

function make_download() {
    //pull all the form fields into a single json object
    var fig_num = $('#figure_number').val();
    var chap_num = $('#chapter_number').val();
    var draft_num = "1st" //make a function to change this!
    var today = current_date();
    
    var cap_title = $('#caption_title').val();
    var cap_text = $('#caption_text').val();
    var title = `Data for Figure ${fig_num} from Chapter ${chap_num} of the ${draft_num} draft of the Working Group I Contribution to the IPCC Seventh Assessment Report (v${today})`;
    var description = `This dataset contains the final data for Figure ${fig_num} from Chapter ${chap_num} of the Working Group I (WGI) Contribution to the Intergovernmental Panel on Climate Change (IPCC) Seventh Assessment Report (AR7). Additional information, including how to cite this figure dataset, is provided in the README.txt alongside the data. 
    Figure ${fig_num} caption: ${cap_title} ${cap_text}
    Links to the figure, report chapter and supplementary material are provided in the Details/Docs section of this catalogue record. Code to reproduce the figure which is hosted on GitHub is also linked, and described under the Process tab below.`;

    var authors = $('#authors input');
    console.log(authors); 
    authors_list  = [];
    // start at author 2 beacuse of the hidden template
    for (var i=2; i < authors.length; i += 2) {
        authors_list.push({"firstname": authors[i].value, "surname": authors[i+1].value});
        console.log(i, authors[i].value);
    }

    var keywords = $('#keywords_input').val();
    console.log(keywords); 
    keywords_list = "IPCC-DDC, IPCC, AR7, WGI, Seventh Assessment Report, Working Group I, Physical Science Basis, Intergovernmental Panel on Climate Change";
    if (keywords) {
        keywords_list = keywords_list.concat(", ", keywords);
    } 
    
    
    var bbox = {"north": $('#north').val(), "south": $('#south').val(),
                "east": $('#east').val(), "west": $('#west').val(),};
    
    
    const timeFormat = $('#time_format').val();

    if (timeFormat === 'CE') {
        const startDate = $('#start_date').val();
        const endDate = $('#end_date').val();

    var timeRange = {
        start: toUTCString($('#start_date').val()),
        end: toUTCString($('#end_date').val())
    };
    }
    else if (timeFormat === 'BP') {
        timeRange = $('#BP_description').val();
    }
    console.log(timeRange);
    
    var docs = $('#docs input');
    var docs_list  = [];
    // start at doc 2 beacuse of the hidden template
    for (var i=2; i < docs.length; i += 2) {
        docs_list.push({"title": docs[i].value, "url": docs[i+1].value});
    }

    var git_link = $('#github_link').val();


    var lineage = "Data produced by Intergovernmental Panel on Climate Change (IPCC) authors and supplied for archiving at the Centre for Environmental Data Analysis (CEDA) by the Technical Support Unit (TSU) for IPCC Working Group I (WGI). \n Data curated on behalf of the IPCC Data Distribution Centre (IPCC-DDC).";
    var quality = "Data as provided by the IPCC";
    var format = "Data are [EDIT] formatted";
    var result_path = `/badc/ar7_wg1/data/ch_${chap_num}/ch${chap_num}_fig${fig_num}/v${today}`;
    var project = {
        "catalogue_url": ""
    };
    var collection = {
        "catalogue_url": ""
    };
    var computation = {
        "catalogue_url": "",
        "title": `Computation for IPCC AR7 figure ${fig_num}`,
        "description": `This figure can be reproduced using the following code: ${git_link} 
Instructions on how to use the code is provided in the repository.`
    };
    var ceda_officer = {
        "firstname": "",
        "surname": ""
    };

    return {"title": title, "description": description, "authors": authors_list, "keywords": keywords_list,
         "logo":238, "bbox": bbox, "time_range": timeRange, "lineage": lineage, "quality": quality,
         "project": project, "collection": collection, "computation": computation, "docs": docs_list,
        "format": format, "result_path": result_path, "ceda_officer": ceda_officer};
}

function current_date() { 
    const d = new Date();
    var year = d.getFullYear();
    var month = String(d.getMonth() + 1).padStart(2, "0");
    var day = String(d.getDate()).padStart(2, "0");
    var date = `${year}${month}${day}`;
    console.log(date);
    return date;
}

function toUTCString(dateString) {
    return new Date(dateString).toISOString().replace('.000', '');
}

function toggle_time_fields() {
    var choice = $('#time_format').val();
    $('#CE_field').toggle(choice === 'CE');
    $('#BP_field').toggle(choice === 'BP');
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

// add a blank row for the authors
add_row("authors");
add_row("docs");
