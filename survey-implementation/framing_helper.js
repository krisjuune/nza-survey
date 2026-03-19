Qualtrics.SurveyEngine.addOnReady(function () {

    console.log("=== NZ HELPER JS START ===");

    function pick(arr, idx) {
        return arr[idx - 1];
    }

    // Loop over all tasks explicitly (1–6)
    for (var t = 1; t <= 6; t++) {

        // --- PRE-PIPE VALUES ---
        var a_fuel = [
            "${e://Field/__js_task1_fuel1_code}",
            "${e://Field/__js_task2_fuel1_code}",
            "${e://Field/__js_task3_fuel1_code}",
            "${e://Field/__js_task4_fuel1_code}",
            "${e://Field/__js_task5_fuel1_code}",
            "${e://Field/__js_task6_fuel1_code}"
        ][t - 1];

        var b_fuel = [
            "${e://Field/__js_task1_fuel2_code}",
            "${e://Field/__js_task2_fuel2_code}",
            "${e://Field/__js_task3_fuel2_code}",
            "${e://Field/__js_task4_fuel2_code}",
            "${e://Field/__js_task5_fuel2_code}",
            "${e://Field/__js_task6_fuel2_code}"
        ][t - 1];

        var a_activity = [
            "${e://Field/__js_task1_activity1_code}",
            "${e://Field/__js_task2_activity1_code}",
            "${e://Field/__js_task3_activity1_code}",
            "${e://Field/__js_task4_activity1_code}",
            "${e://Field/__js_task5_activity1_code}",
            "${e://Field/__js_task6_activity1_code}"
        ][t - 1];

        var b_activity = [
            "${e://Field/__js_task1_activity2_code}",
            "${e://Field/__js_task2_activity2_code}",
            "${e://Field/__js_task3_activity2_code}",
            "${e://Field/__js_task4_activity2_code}",
            "${e://Field/__js_task5_activity2_code}",
            "${e://Field/__js_task6_activity2_code}"
        ][t - 1];

        var a_durability = [
            "${e://Field/__js_task1_durability1_code}",
            "${e://Field/__js_task2_durability1_code}",
            "${e://Field/__js_task3_durability1_code}",
            "${e://Field/__js_task4_durability1_code}",
            "${e://Field/__js_task5_durability1_code}",
            "${e://Field/__js_task6_durability1_code}"
        ][t - 1];

        var b_durability = [
            "${e://Field/__js_task1_durability2_code}",
            "${e://Field/__js_task2_durability2_code}",
            "${e://Field/__js_task3_durability2_code}",
            "${e://Field/__js_task4_durability2_code}",
            "${e://Field/__js_task5_durability2_code}",
            "${e://Field/__js_task6_durability2_code}"
        ][t - 1];

        // --- CLASSIFICATION ---
        function isNetZeroAligned(fuel, activity, durability) {
            return (
                fuel === "plants" ||
                fuel === "electric" ||
                (
                    (activity === "trees" || activity === "direct_air") &&
                    durability === "permanent"
                )
            );
        }

        var a_nz = isNetZeroAligned(a_fuel, a_activity, a_durability);
        var b_nz = isNetZeroAligned(b_fuel, b_activity, b_durability);

        // --- STORE ---
        Qualtrics.SurveyEngine.setJSEmbeddedData("__js_a_nz_binary_task" + t, a_nz ? 1 : 0);
        Qualtrics.SurveyEngine.setJSEmbeddedData("__js_b_nz_binary_task" + t, b_nz ? 1 : 0);

        console.log("Task", t, "A NZ:", a_nz, "B NZ:", b_nz);
    }

    console.log("=== NZ HELPER JS END ===");
});
