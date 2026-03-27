Qualtrics.SurveyEngine.addOnload(function() {

    const language = "${e://Field/Q_Language}".trim().toUpperCase();
    const isArabic = language === "AR";
    const totalLoops = 6;
    let currentLoop = parseInt("${lm://CurrentLoopNumber}");
    if (isNaN(currentLoop)) currentLoop = 1;

    // ------------------------
    // Task counter
    // ------------------------
    const counters = {
        1: { en: "${e://Field/__js_task1_counter}", ar: "${e://Field/__js_task1_counter_ar}" },
        2: { en: "${e://Field/__js_task2_counter}", ar: "${e://Field/__js_task2_counter_ar}" },
        3: { en: "${e://Field/__js_task3_counter}", ar: "${e://Field/__js_task3_counter_ar}" },
        4: { en: "${e://Field/__js_task4_counter}", ar: "${e://Field/__js_task4_counter_ar}" },
        5: { en: "${e://Field/__js_task5_counter}", ar: "${e://Field/__js_task5_counter_ar}" },
        6: { en: "${e://Field/__js_task6_counter}", ar: "${e://Field/__js_task6_counter_ar}" },
    };

    const counter = isArabic ? counters[currentLoop].ar : counters[currentLoop].en;

    const titleSpan = document.querySelector(".task-title");
    if (titleSpan) {
        titleSpan.textContent = "${e://Field/__js_task_name}" + " " + counter;
    }

    // ------------------------
    // RTL layout
    // ------------------------
    const container = document.getElementById("tableContainer");
    if (container) {
        container.style.direction = isArabic ? "rtl" : "ltr";
        container.style.textAlign = isArabic ? "right" : "center";

        document.querySelectorAll(".wholetable td.attr, .wholetable td.name")
            .forEach(function(td) {
                td.style.textAlign = isArabic ? "right" : "left";
            });
    }

});