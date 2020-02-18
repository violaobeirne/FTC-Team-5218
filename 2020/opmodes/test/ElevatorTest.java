package opmodes.test;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import opmodes.calibration.MiyazakiCalibration;
import team25core.DeadmanMotorTask;
import team25core.GamepadTask;
import team25core.RobotEvent;
import team25core.Robot;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 2/8/2020.
 */
@TeleOp(name = "Elevator Test")
public class ElevatorTest extends Robot {
    // vertical lift code
    private DcMotor vLift;
    private int stoneLevelEncoderValue;
    private int levelCounter = 0;
    private int liftDistance = 0;

    @Override
    public void init ()
    {
        // vertical lift goodness
        vLift = hardwareMap.dcMotor.get("vLift");
        stoneLevelEncoderValue = 300;
        vLift.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        vLift.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
    }

    @Override
    public void handleEvent (RobotEvent e)
    {

    }

    @Override
    public void start ()
    {
        DeadmanMotorTask liftUp = new DeadmanMotorTask(this, vLift, MiyazakiCalibration.VLIFT_UP, GamepadTask.GamepadNumber.GAMEPAD_1, DeadmanMotorTask.DeadmanButton.RIGHT_BUMPER);
        addTask(liftUp);
        DeadmanMotorTask liftDown = new DeadmanMotorTask(this, vLift, MiyazakiCalibration.VLIFT_DOWN, GamepadTask.GamepadNumber.GAMEPAD_1, DeadmanMotorTask.DeadmanButton.RIGHT_TRIGGER);
        addTask(liftDown);

        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                switch (event.kind) {
                    case BUTTON_A_DOWN:
                        RobotLog.i("163 %d", vLift.getCurrentPosition());
                        resetVLift();
                        break;
                        // vLift.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
                        // vLift.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
                        // vLift.setPower(MiyazakiCalibration.VLIFT_DOWN);
                        /*
                        if (Math.abs(vLift.getCurrentPosition()) > 20) {
                            RobotLog.i("163: Vertical lift moving down.");
                            vLift.setPower(MiyazakiCalibration.VLIFT_DOWN);
                        }

                         */
                    case BUTTON_B_DOWN:
                        increaseLevel();
                        break;
                }
            }
        });
    }

    public void recordDistance()
    {
        this.addTask(new SingleShotTimerTask(this, 1000) {
            public void handleEvent(RobotEvent e) {
                liftDistance += vLift.getCurrentPosition();
                RobotLog.i("Level Increased %d", liftDistance);
            }
        });
    }

    public void increaseLevel ()
    {
        levelCounter++;
        this.addTask(new RunToEncoderValueTask(this, vLift, stoneLevelEncoderValue, MiyazakiCalibration.VLIFT_UP) {
            public void handleEvent(RobotEvent e) {
                RunToEncoderValueEvent event = (RunToEncoderValueEvent) e;
                switch(event.kind) {
                    case DONE:
                        recordDistance();
                        break;
                }
            }
        });
    }

    public void resetVLift ()
    {
        RobotLog.i("163:%d", liftDistance);
        this.addTask(new RunToEncoderValueTask(this, vLift, Math.abs(liftDistance), MiyazakiCalibration.VLIFT_DOWN) {
            public void handleEvent(RobotEvent e) {
                RunToEncoderValueEvent event = (RunToEncoderValueEvent) e;
                switch(event.kind) {
                    case DONE:
                        RobotLog.i("Back to zero");
                        levelCounter = 0;
                        liftDistance = 0;
                        break;
                }
            }
        });
    }

    /*
    @Override
    public void loop ()
    {
        super.loop();
        if (descending == true) {
            if (Math.abs(vLift.getCurrentPosition()) > stoneLevelEncoderValue * levelCounter) {
                RobotLog.i("163: Vertical lift stopped.");
                vLift.setPower(0.0);
                levelCounter = 0;
                descending = false;
            }
        }
    }
     */
}
