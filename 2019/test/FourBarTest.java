package test;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;

import opmodes.Utilities.VivaldiCalibration;
import team25core.DeadmanMotorTask;
import team25core.GamepadTask;
import team25core.HoldPositionTask;
import team25core.MotorStallTask;
import team25core.Robot;
import team25core.RobotEvent;

/**
 * Created by Lizzie on 12/29/2018.
 */
@TeleOp(name = "Four Bar Test")
public class FourBarTest extends Robot {

    private DcMotor fourBar;
    private DcMotor bungeeBox;
    private HoldPositionTask holdPositionTask;
    private MotorStallTask motorStallTask;
    private double fourBarDeadBand = 0;

    @Override
    public void init() {
        fourBar = hardwareMap.dcMotor.get("fourBar");
        bungeeBox = hardwareMap.dcMotor.get("bungeeBox");
        bungeeBox.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        bungeeBox.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        fourBar.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        fourBar.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        holdPositionTask = new HoldPositionTask(this, fourBar, fourBarDeadBand);
        motorStallTask = new MotorStallTask(this, fourBar, telemetry);
    }

    @Override
    public void start() {
        DeadmanMotorTask raiseFourBar = new DeadmanMotorTask(this, fourBar, VivaldiCalibration.FOUR_BAR_UP, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.LEFT_BUMPER);
        addTask(raiseFourBar);
        DeadmanMotorTask lowerFourBar = new DeadmanMotorTask(this, fourBar, VivaldiCalibration.FOUR_BAR_DOWN, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.LEFT_TRIGGER);
        addTask(lowerFourBar);

        DeadmanMotorTask deployBungeeBox = new DeadmanMotorTask(this, bungeeBox, VivaldiCalibration.BUNGEE_BOX_DEPLOY, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_A) {
            @Override
            public void handleEvent(RobotEvent e) { }
        };
        addTask(deployBungeeBox);

        DeadmanMotorTask stowBungeeBox = new DeadmanMotorTask(this, bungeeBox, VivaldiCalibration.BUNGEE_BOX_STOW, GamepadTask.GamepadNumber.GAMEPAD_2, DeadmanMotorTask.DeadmanButton.BUTTON_B) {
        @Override
        public void handleEvent(RobotEvent e) { }
         };
        addTask(stowBungeeBox);
        addTask(motorStallTask);
    }

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof DeadmanMotorTask.DeadmanMotorEvent) {
            DeadmanMotorTask.DeadmanMotorEvent event = (DeadmanMotorTask.DeadmanMotorEvent) e;
            switch (event.kind) {
                case DEADMAN_BUTTON_DOWN:
                    this.removeTask(holdPositionTask);
                    break;
                case DEADMAN_BUTTON_UP:
                    this.addTask(holdPositionTask);
                    break;
            }
        }
    }
}
