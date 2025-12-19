 function handleSelection() {
            const selector = document.getElementById('actionSelector');
            const choice = selector.value;
            const overlay = document.getElementById('game-status');
            var playerId = FBInstant.player.getID();

            if (!choice || choice === "") return;

            // Logic based on selection
            if (choice === "FBInstant.getPlatform") {
                displayAndConsoleLog(choice, FBInstant.getPlatform());
            } else if (choice === "FBInstant.getSDKVersion") {
                displayAndConsoleLog(choice, FBInstant.getSDKVersion());
            } else if (choice === "FBInstant.getSupportedAPIs") {
                displayAndConsoleLog(choice, FBInstant.getSupportedAPIs());
            } else if (choice === "logEvent.purchase") {
                FBInstant.logEvent("purchase",
                    199,
                    { 'fb_content_id': 'gold_pack_199', 'fb_content_type': 'product', 'fb_currency': 'USD' }
                );
                displayAndConsoleLog(choice, "");
            }
            else if (choice === "logEvent.level_complete") {
                FBInstant.logEvent("level_complete",
                    1,
                    { 'fb_level': 1 }
                );
                displayAndConsoleLog(choice, "");
            }
            else if (choice === "logEvent.custom_event") {
                FBInstant.logEvent("custom_event", 42, { custom_property: 'custom_value' });
                displayAndConsoleLog(choice, "");
            }

            setTimeout(() => {
                overlay.innerText = "Hello back, " + playerId;
            }, 1500);
        }

        function displayAndConsoleLog(choice, value) {
            const overlay = document.getElementById('game-status');
            overlay.innerText = statusFormatter(choice, value);
            console.log(choice + ": " + value);
        }

        function statusFormatter(choice, value) {
            return choice + ": " + value;
        }