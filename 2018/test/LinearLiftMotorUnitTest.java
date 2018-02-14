package test;

import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import opmodes.HisaishiCalibration;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;

/**
 * Created by admin on 12/15/2017.
 */

@TeleOp(name="5218 Lift Motor Test")
@Disabled
public class LinearLiftMotorUnitTest extends Robot {

    DcMotor glyphElevator;
    private Servo glyphLGrabber;
    private Servo glyphRGrabber;

    @Override
    public void handleEvent(RobotEvent e)
    {
        // Keep scrolling!
    }

    @Override
    public void init()
    {
        glyphElevator = hardwareMap.dcMotor.get("glyphElevator");
        glyphLGrabber = hardwareMap.servo.get("glyphLeftGrabber");
        glyphRGrabber = hardwareMap.servo.get("glyphRightGrabber");
    }

    @Override
    public void start()
    {
        glyphLGrabber.setPosition(HisaishiCalibration.GLYPH_CLOSE_LEFT_POSITION);
        glyphRGrabber.setPosition(HisaishiCalibration.GLYPH_CLOSE_RIGHT_POSITION);
        addTask(new SingleShotTimerTask(this, 400) {
            @Override
            public void handleEvent(RobotEvent e)
            {
                SingleShotTimerTask.SingleShotTimerEvent event = (SingleShotTimerTask.SingleShotTimerEvent)e;
                if (event.kind == SingleShotTimerTask.EventKind.EXPIRED) {
                }
            }
        });
        glyphElevator.setPower(0.6);
        addTask(new SingleShotTimerTask(this, 400) {
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
