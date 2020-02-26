package opmodes.test;

import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.TouchSensor;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import opmodes.ElevatorTask;
import opmodes.calibration.MiyazakiCalibration;
import team25core.DeadmanMotorTask;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;
import team25core.TouchSensorCriteria;

/**
 * Created by Lizzie on 2/8/2020.
 */
@TeleOp(name = "CHAOS")
@Disabled
public class CHAOSTest extends Robot {

    // vertical lift code
    private DcMotor vLift;
    private TouchSensor liftTouchSensor;
    private ElevatorTask elevatorTask;
    private int targetLevelValue = 0;
    private Telemetry.Item targetLevelItem;

    @Override
    public void init ()
    {
        vLift = hardwareMap.dcMotor.get("vLift");
        liftTouchSensor = hardwareMap.touchSensor.get("liftTouch");
        vLift.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        vLift.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        elevatorTask = new ElevatorTask(this, vLift, liftTouchSensor);
        addTask(elevatorTask);

        targetLevelItem = telemetry.addData("Target Level: ", "NOT STARTED");
    }

    @Override
    public void handleEvent (RobotEvent e)
    {

    }

    @Override
    public void start ()
    {
        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                switch (event.kind) {
                    case RIGHT_BUMPER_DOWN:
                        elevatorTask.levelUp();
                        targetLevelItem.setValue(elevatorTask.getTargetLevel());
                        break;
                    case RIGHT_TRIGGER_DOWN:
                        targetLevelValue = 0;
                        elevatorTask.fullyRetractLift();
                        targetLevelItem.setValue(elevatorTask.getTargetLevel());
                        break;
                    case DPAD_DOWN_DOWN:
                        if (targetLevelValue > 0)
                        {
                            targetLevelValue -= 1;
                        }
                        elevatorTask.setTargetLevel(targetLevelValue);
                        targetLevelItem.setValue(elevatorTask.getTargetLevel());
                        break;
                    case DPAD_UP_DOWN:
                        targetLevelValue += 1;
                        elevatorTask.setTargetLevel(targetLevelValue);
                        targetLevelItem.setValue(elevatorTask.getTargetLevel());
                        break;
                }
            }
        });
    }
}
