package test;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;

import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;

/**
 * Created by admin on 12/15/2017.
 */

@TeleOp(name="5218 Lift Motor Test")
public class LinearLiftMotorUnitTest extends Robot {

    DcMotor glyphElevator;

    @Override
    public void handleEvent(RobotEvent e)
    {
        // Keep scrolling!
    }

    @Override
    public void init()
    {
        glyphElevator = hardwareMap.dcMotor.get("glyphElevator");
    }

    @Override
    public void start()
    {
        glyphElevator.setPower(0.6);
        addTask(new SingleShotTimerTask(this, 600) {
            @Override
            public void handleEvent(RobotEvent e)
            {
                SingleShotTimerEvent event = (SingleShotTimerEvent)e;
                if (event.kind == EventKind.EXPIRED) {
                    glyphElevator.setPower(0);
                }
            }
        });
    }
}
