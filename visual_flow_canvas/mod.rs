use egui::{Ui, WidgetText};
use crate::{Plugin, AppCommand, TabInstance, Tab};

// ----------------------------------------------------------------------------
// Tab Instance
// ----------------------------------------------------------------------------

#[derive(Clone, Debug, Default)]
pub struct VisualFlowCanvasTab;

impl TabInstance for VisualFlowCanvasTab {
    fn title(&self) -> WidgetText {
        "VisualFlowCanvas".into()
    }

    fn ui(&mut self, ui: &mut Ui, _control: &mut Vec<AppCommand>) {
        ui.centered_and_justified(|ui| {
            ui.label("[empty]");
        });
    }

    fn box_clone(&self) -> Box<dyn TabInstance> {
        Box::new(self.clone())
    }
}

// ----------------------------------------------------------------------------
// Plugin Implementation
// ----------------------------------------------------------------------------

pub struct VisualFlowCanvasPlugin;

impl Plugin for VisualFlowCanvasPlugin {
    fn name(&self) -> &str {
        crate::plugins::PLUGIN_NAME_VISUAL_FLOW_CANVAS
    }

    fn on_tab_menu(&mut self, ui: &mut Ui, control: &mut Vec<AppCommand>) {
        if ui.button("VisualFlowCanvas").clicked() {
            control.push(AppCommand::OpenTab(Tab::new(Box::new(VisualFlowCanvasTab::default()))));
            ui.close_menu();
        }
    }
}

pub fn create() -> VisualFlowCanvasPlugin {
    VisualFlowCanvasPlugin
}
