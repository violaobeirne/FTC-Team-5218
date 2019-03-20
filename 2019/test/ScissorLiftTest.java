package test;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.util.RobotLog;

import opmodes.Utilities.VivaldiCalibration;
import team25core.GamepadTask;
import team25core.MonitorMotorTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;

/**
 * Created by Lizzie on 11/24/2018.
 */
@TeleOp(name = "Scissor Lift Test")
public class ScissorLiftTest extends Robot {
    private DcMotor liftLeft;
    private DcMotor liftRight;

    @Override
    public void init() {
        liftLeft = hardwareMap.dcMotor.get("liftLeft");
        liftRight = hardwareMap.dcMotor.get("liftRight");
        liftLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        liftRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
    }

    public void liftUp()
    {
        RobotLog.i("251 - Going UP");
        RunToEncoderValueTask leftTask = new RunToEncoderValueTask(this, liftLeft, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_LEFT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    RobotLog.i("251 - DONE L");
                    liftLeft.setPower(0.0);
                }
            }
        };
        RunToEncoderValueTask rightTask = new RunToEncoderValueTask(this, liftRight, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_RIGHT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    RobotLog.i("251 - DONE R");
                    liftRight.setPower(0.0);
                }
            }
        };
        addTask(leftTask);
        addTask(rightTask);
    }

    public void liftDown()
    {
        RobotLog.i("251 - Going DOWN");
        RunToEncoderValueTask leftTask = new RunToEncoderValueTask(this, liftLeft, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_LEFT_DOWN) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    RobotLog.i("251 - DONE L");
                    liftLeft.setPower(VivaldiCalibration.LIFT_STOP);
                }
            }
        };
        RunToEncoderValueTask rightTask = new RunToEncoderValueTask(this, liftRight, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_RIGHT_DOWN) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    RobotLog.i("251 - DONE R");
                    liftRight.setPower(VivaldiCalibration.LIFT_STOP);
                }
            }
        };
        addTask(leftTask);
        addTask(rightTask);
    }

    @Override
    public void start() {
       this.addTask(new MonitorMotorTask(this, liftLeft));
       this.addTask(new MonitorMotorTask(this, liftRight));
       this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
           public void handleEvent(RobotEvent e) {
               GamepadEvent event = (GamepadEvent) e;
               if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
                   liftRight.setPower(VivaldiCalibration.LIFT_RIGHT_UP);
               } else if (event.kind == EventKind.RIGHT_TRIGGER_DOWN) {
                   liftRight.setPower(VivaldiCalibration.LIFT_RIGHT_DOWN);
               } else if (event.kind == EventKind.LEFT_BUMPER_DOWN) {
                   liftLeft.setPower(VivaldiCalibration.LIFT_LEFT_UP);
               } else if (event.kind == EventKind.LEFT_TRIGGER_DOWN) {
                   liftLeft.setPower(VivaldiCalibration.LIFT_LEFT_DOWN);
               } else if (event.kind == EventKind.BUTTON_X_DOWN) {
                   liftLeft.setPower(VivaldiCalibration.LIFT_LEFT_DOWN);
                   liftRight.setPower(VivaldiCalibration.LIFT_RIGHT_DOWN);
               } else if (event.kind == EventKind.BUTTON_Y_DOWN) {
                   liftLeft.setPower(VivaldiCalibration.LIFT_LEFT_UP);
                   liftRight.setPower(VivaldiCalibration.LIFT_RIGHT_UP);
               } else if (event.kind == EventKind.BUTTON_X_UP || event.kind == EventKind.BUTTON_Y_UP) {
                   liftLeft.setPower(VivaldiCalibration.LIFT_STOP);
                   liftRight.setPower(VivaldiCalibration.LIFT_STOP);
               } else if (event.kind == EventKind.BUTTON_A_DOWN) {
                   liftDown();
               } else if (event.kind == EventKind.RIGHT_BUMPER_DOWN) {
               } else if (event.kind == EventKind.BUTTON_B_DOWN) {
                   liftUp();
               }

           }
       });

    }

    @Override
    public void handleEvent(RobotEvent e) {

    }
}
