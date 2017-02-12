package test;

/*
 * FTC Team 25: izzielau, October 27, 2015
 */

import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DeviceInterfaceModule;

import team25core.LimitSwitchTask;
import team25core.Robot;
import team25core.RobotEvent;

@TeleOp(name = "Mocha Limit Switches", group = "5218")
@Disabled
public class CoreLimitSwitchTest extends Robot
{

    public DeviceInterfaceModule interfaceModule;

    @Override
    public void handleEvent(RobotEvent e)
    {

    }

    @Override
    public void init()
    {
        interfaceModule = hardwareMap.deviceInterfaceModule.get("interface");
    }

    @Override
    public void start()
    {
        addTask(new LimitSwitchTask(this, interfaceModule, 0) {
            @Override
            public void handleEvent(RobotEvent e)
            {
                LimitSwitchEvent event = (LimitSwitchEvent)e;

                if (event.kind == EventKind.CLOSED) {
                    telemetry.addData("Status (1): ", "closed");
                } else if (event.kind == EventKind.OPEN) {
                    telemetry.addData("Status (1): ", "open");
                } else {
                    telemetry.addData("Status (1): ", "unknown");
                }
            }
        });

        addTask(new LimitSwitchTask(this, interfaceModule, 1) {
            @Override
            public void handleEvent(RobotEvent e)
            {
                LimitSwitchEvent event = (LimitSwitchEvent)e;

                if (event.kind == EventKind.CLOSED) {
                    telemetry.addData("Status (2): ", "closed");
                } else if (event.kind == EventKind.OPEN) {
                    telemetry.addData("Status (2): ", "open");
                } else {
                    telemetry.addData("Status (2): ", "unknown");
                }
            }
        });
    }
}